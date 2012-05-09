// vim: set sts=4: ts=4: sw=4: et
/*
 (c) PPershing
 (c) original file from: http://www.ridefreebikemaps.com/
 */

var map;
var directionsService = new google.maps.DirectionsService;
var elevationService = {
    elevation : new google.maps.ElevationService(),
    queue : [],
    DELAY: 100,
    timer: null,
    getElevationAlongPath : function(request, callback) {
        this.queue.push([request, callback]);
        if (this.timer == null) {
            this.timer = setInterval(this.processQueue.bind(this), this.DELAY);
        }
    },
    processQueue: function() {
        if (this.queue.length > 0) {
            // we need only last element of the queue
            var data = this.queue[this.queue.length - 1];
            this.queue = [] 
            var request = data[0];
            var callback = data[1];
            this.elevation.getElevationAlongPath(request, callback);
        } else {
            clearInterval(this.timer);
            this.timer = null;
        }
    },
};

var geocoderService = {
    geocoder : new google.maps.Geocoder(),
    queue : [],
    DELAY: 2000,
    timer: null,
    geocode: function(request, callback) {
        this.queue.push([request, callback]);
        if (this.timer == null) {
            this.timer = setInterval(this.processQueue.bind(this), this.DELAY);
        }
    },
    processQueue: function() {
        if (this.queue.length > 0) {
            var data = this.queue.splice(0, 1)[0];
            var request = data[0];
            var callback = data[1];
            this.geocoder.geocode(request, callback);
        } else {
            clearInterval(this.timer);
            this.timer = null;
        }
    },
};


var mousemarker = null;
var elevationChart = null;
var slopeChart = null;

var sections = [];

var elevations = [];
var slopeToElevationIndex = [];

var CONFIG = {
    MAP_SETTINGS : {
        zoom: 8,
        center: new google.maps.LatLng(46.6, 7.7),
        mapTypeId: google.maps.MapTypeId.HYBRID
    },
    SPEED_SETTINGS : {
        SPEED_KM_FLAT: 22 /*km/h*/,
        ASCENT_METRES_MIN: 15 /*m/min*/,
    },
    DEFAULT_TRAVEL_MODE: google.maps.DirectionsTravelMode.DRIVING,
};

var ELEMENTS = {
    mapCanvas: document.getElementById("map_canvas"),
    elevationChart: document.getElementById("elevation_chart"),
    slopeChart: document.getElementById("slope_chart"),
    totalDistance: document.getElementById("total_distance"),
    totalAscent: document.getElementById("total_ascent"),
    estimatedTime: document.getElementById("estimated_time"),
    permalinkEdit: document.getElementById("permalink"),
    savedList: document.getElementById("saved_trips_list"),
    saveTripButton: document.getElementById("save_trip_button"),
    saveTripTextName: document.getElementById("save_trip_name"),
    directionsPanel: document.getElementById("directions_panel"),
    autoNameCheckbox: document.getElementById("auto_name_checkbox"),
    clearButton: document.getElementById("clear_button"),
    serverTripsList: document.getElementById("server_trips_list"),
}

function refreshDirections() {
    var rootElement = ELEMENTS.directionsPanel;

    // Beware, indexes are shifting!
    while (rootElement.children.length) {
      rootElement.removeChild(rootElement.children[0]);
    }

    for (var i = 0; i < sections.length; ++i) {
      var name = sections[i].geocode;

      // the entire entry
      var entry = document.createElement("li");
      var label = document.createElement("span");
      var select = createTravelModeSelect(sections[i].travelMode);
      select.onchange = function(select, x) {
        return function() {
            sections[x].travelMode = select.value;
            findDirections(x);
            refreshPermalink();
        };
      }(select, i);
      label.textContent = name;

      entry.appendChild(label);
      entry.appendChild(select);
      rootElement.appendChild(entry);
    };
    if (ELEMENTS.autoNameCheckbox.checked && sections.length) {
        ELEMENTS.saveTripTextName.value = sections[0].geocode + ' -> ' +
            sections[sections.length-1].geocode;
    }
}

function initializeUI() {
    map = new google.maps.Map(ELEMENTS.mapCanvas, CONFIG.MAP_SETTINGS);
    google.maps.event.addListener(map, "click", function (event) {addMarker(event.latLng, sections.length);});
    google.maps.event.addListener(map, "bounds_changed", function (event) {refreshPermalink()});

    ELEMENTS.clearButton.addEventListener("click", clearMarkers);
    ELEMENTS.saveTripButton.addEventListener("click", 
        function() {saved_trips.add({name: ELEMENTS.saveTripTextName.value, data:getCurrentState()});}
      );

    elevationChart = new google.visualization.AreaChart(ELEMENTS.elevationChart);
    google.visualization.events.addListener(elevationChart, 'onmouseover', elevationMouseOver);
    google.visualization.events.addListener(elevationChart, 'onmouseout', elevationMouseOut);
    google.visualization.events.addListener(elevationChart, 'select', elevationClick);

    slopeChart = new google.visualization.ColumnChart(ELEMENTS.slopeChart);
    google.visualization.events.addListener(slopeChart, 'onmouseover', slopeMouseOver);
    google.visualization.events.addListener(slopeChart, 'onmouseout', slopeMouseOut);

}

function elevationMouseOver(e) {
    elevationChart.setSelection([e]);
    if (mousemarker == null) {
        mousemarker = new google.maps.Marker({
            map: map,
            icon: "http://maps.google.com/mapfiles/ms/icons/green-dot.png"
        });
    }
    mousemarker.setPosition(elevations[e.row].location);
    elevationChartLastPos = e.row;
}

function elevationMouseOut(e) {
    elevationChart.setSelection([]);
    mousemarker.setMap(null);
    mousemarker = null;
}

function slopeMouseOver(e) {
    var selection = [];
    var start = slopeToElevationIndex[e.row];
    var end = slopeToElevationIndex[e.row + 1]
    for (var x = start; x < end; x++) {
        selection.push({'row': x, 'column': 1});
    }
    elevationChart.setSelection(selection);
}

function elevationClick() {
    var nearest = findNearestVertex(elevations[elevationChartLastPos].location);
    if (nearest) {
        addMarker(nearest[0], nearest[1] + 1);
    }
}

function slopeMouseOut(e) {
    elevationChart.setSelection([])
}

function directionsLoaded(marker_index, result) {
    var latlngs = [];

    for (var x = 0; x < result.steps.length; x++) {
        for (var y = 0; y < result.steps[x].lat_lngs.length; y++) {
            latlngs.push(result.steps[x].lat_lngs[y]);
        }
    }

    var a = {
        path: latlngs,
        strokeColor: "#0000CC",
        strokeOpacity: 0.4,
        map: map
    };

    sections[marker_index].polyline.setMap(null);
    sections[marker_index].polyline = new google.maps.Polyline(a);
    sections[marker_index].route = result;

    google.maps.event.addListener(sections[marker_index].polyline, "click", function (a) {
        var nearest = findNearestVertex(a.latLng);
        addMarker(nearest[0], nearest[1] + 1);
    });
    var a = totalDistance();
    ELEMENTS.totalDistance.textContent = "Distance: " + Math.round(a / 1000) + " km";
    updateElevation()
}

function markerIndex(marker) {
    for (var x = 0; x < sections.length; x++) {
        if (sections[x].marker == marker) return x;
    }
    return -1;
}

function removeMarker(marker) {
    var marker_index = markerIndex(marker);
    utils.assert(marker_index != -1, "marker already deleted!");

    removed = sections.splice(marker_index, 1);
    removed[0].polyline.setMap(null);
    removed[0].marker.setMap(null);

    // Note: we erased 1 item!
    if (marker_index != 0) {
        if (marker_index == sections.length) {
            sections[marker_index - 1].polyline.setMap(null);
        } else {
            findDirections(marker_index - 1);
        }
    };
    refreshDirections();
    refreshPermalink();
}

function dragendMarker(marker) {
    var marker_index = markerIndex(marker);
    utils.assert(marker_index != -1, "unknown marker!");

    if (marker_index == 0) {
        sections[0].polyline.setMap(null);
        if (sections.length > 1) {
            findDirections(0);
        }
    } else if (marker_index == sections.length - 1) {
        sections[marker_index-1].polyline.setMap(null);
        if (sections.length > 1) {
            findDirections(marker_index - 1);
        }
    } else {
        findDirections(marker_index - 1);
        findDirections(marker_index);
    }

    refreshDirections();
    refreshPermalink();
    geocode(marker_index);
}


function addMarker(position, index) {
    if (!position) return;

    var marker = new google.maps.Marker({
        position: position,
        map: map,
        draggable: true
    });

    google.maps.event.addListener(marker, "click", function () {
        removeMarker(marker);
    });
    google.maps.event.addListener(marker, "dragend", function () {
        dragendMarker(marker);
    });

    var section = {
        marker: marker,
        polyline: new google.maps.Polyline(),
        travelMode: (index > 0) ? 
                sections[index - 1].travelMode :
                CONFIG.DEFAULT_TRAVEL_MODE,
        geocode: null,
        route: null,
    };
    if (index != sections.length) {
        sections.splice(index, 0, section);
        // invalidated sections
        sections[index+1].polyline.setMap(null);
        sections[index+1].polyline = new google.maps.Polyline();
        sections[index+1].route = null;
    } else {
        sections.push(section);
    }

    if (index > 0) {
        findDirections(index - 1);
    }
    if (index < sections.length - 1) {
        findDirections(index);
    }

    refreshPermalink();

    geocode(sections.length - 1);
}

function geocode(index) {
    utils.assert(index >= 0 && index < sections.length);

    var point = sections[index].marker.getPosition();

    geocoderService.geocode(
      { location: point },
      function(results, status) {
        if (index >= sections.length ||
            sections[index].marker.getPosition() != point) {
            console.log('geocode: stale callback!');
            return;
        }
        if (status == "OK") {
            sections[index].geocode = results[0].formatted_address;
        } else {
            sections[index].geocode = "unknown(" + status + ")";
        }
        refreshDirections();
      });
}

function createTravelModeSelect(selected_mode) {
  var select = document.createElement("select");
  options = [ ["STRAIGHT", "Straight link"],
              [google.maps.DirectionsTravelMode.WALKING, "Walk"],
              [google.maps.DirectionsTravelMode.DRIVING, "Car"],
            ]
  for (var i=0; i<options.length; i++) {
    var option = document.createElement("option");
    option.selected = (selected_mode == options[i][0]);
    option.value = options[i][0];
    option.textContent=options[i][1];
    select.appendChild(option);
  }
  return select;
}

function findDirections(x) {
    var y = x + 1; // next point
    utils.assert(x >= 0 && x < sections.length, "out of bounds");
    if (sections[x].polyline) {
        sections[x].polyline.setMap(null);
    }
    if (sections.length == y) {
        // nothing to do (dummy segment at the end)
        console.log('findDirections: skipped last segment');
        return;
    }

    var start_point = sections[x].marker.getPosition();
    var end_point = sections[y].marker.getPosition();
    var travel_mode = sections[x].travelMode;
    var straight_result = {
        steps: [{
            lat_lngs: [start_point, end_point],
        }],
        distance: {
            value: start_point.distanceTo(end_point),
        },
    };
    
    var a = {
        map: map,
        path: [start_point, end_point],
    };
    sections[x].polyline.setMap(null);
    // pre-set straight line
    sections[x].polyline = new google.maps.Polyline(a);

    if (travel_mode == "STRAIGHT") {
        directionsLoaded(x, straight_result);
        return;
    };

    var query = {
        origin: start_point,
        destination: end_point,
        travelMode: travel_mode,
        avoidHighways: true
    };

    directionsService.route(query, function (directions_result, directions_status) {
        utils.assert(directions_result.routes.length == 1);
        utils.assert(directions_result.routes[0].legs.length == 1);
        var result = directions_result.routes[0].legs[0];

        if (x >= sections.length ||
            y >= sections.length || 
            start_point != sections[x].marker.getPosition() ||
            end_point != sections[y].marker.getPosition() ||
            travel_mode != sections[x].travelMode) {
          console.log('findDirections: stale callback!');
          return;
        }
        if (directions_status == "OK") {
            directionsLoaded(x, result);
        } else {
            // fallback to a straight line in case of an error
            directionsLoaded(x, straight_result);
        }
    });
}

function clearMarkers() {
    for (x = 0; x < sections.length; x++) {
        sections[x].marker.setMap(null);
        sections[x].polyline.setMap(null);
    }
    sections = [];
    // Due to some unknown reason it might be null even after initialization
    plotElevation([]);
    plotSlope([]);
    refreshPermalink();
    refreshDirections();
    ELEMENTS.saveTripTextName.value = "";
}

function getTrackPoints(max_points) {
    var track = []
    for (var segment = 0; segment < sections.length-1; segment++) {
        var latLngs = sections[segment].polyline.getPath();
        var step = Math.max(1, latLngs.length / (max_points / sections.length));
        for (var x = 0; x < latLngs.length; x+=step) {
            track.push(latLngs.getAt(Math.floor(x)))
        }
    }
    return track;
}

function updateElevation() {
    var MAX_POINTS = 100;
    var track = getTrackPoints(MAX_POINTS);
    // TODO: this is nasty!
    var fingerprint = JSON.stringify(track);
    // note, we do not want last section!
    if (track.length == 0) {
        ELEMENTS.totalAscent.textContent = "total ascent: N/A"
        plotElevation([]);
        plotSlope([]);
        return;
    }


    var query = {
        path: track,
        samples: 250
    }

    elevationService.getElevationAlongPath(query, function(a, status) {
        var track2 = getTrackPoints(MAX_POINTS);
        var fingerprint2 = JSON.stringify(track2)
        if (fingerprint != fingerprint2) {
            console.log('updateElevation: stale callback!');
            return;
        }
        if (status == google.maps.ElevationStatus.OK) {
            // FIXME: this needs to be global due to the stupid graph!
            elevations = a;
            plotElevation(elevations)
            plotSlope(elevations)
            updateAscentInfo(elevations)
        } else {
            plotElevation([]);
            plotSlope([]);
            ELEMENTS.totalAscent.textContent = "error retrieving elevation info!";
        }
    });
}

function totalDistance() {
    for (d = totdistance = 0; d < sections.length; d++) {
        if (sections[d].route) totdistance += sections[d].route.distance.value;
    }
    return totdistance;
}

function plotElevation(elevations) {
        var totdistance = totalDistance();
        var c = new google.visualization.DataTable();
        c.addColumn("number", "Distance");
        c.addColumn("number", "Elevation");
        for (var e = 0; e < elevations.length; e++) {
            var distance = e / elevations.length * totdistance / 1000.0;
            c.addRow([Math.round(distance * 100)/100,
                      Math.round(elevations[e].elevation)]);
        }

        elevationChart.draw(c, {
            width: 700,
            height: 250,
            series: [{visibleInLegend: false}],
        })
}

function plotSlope(elevations) {
        var totdistance = totalDistance();

        var c2 = new google.visualization.DataTable;
        c2.addColumn("string", "Dist");
        c2.addColumn("number", "Slope");
        distSkip = 1 + Math.floor(elevations.length * 1000 / totdistance);

        slopeToElevationIndex = [0]

        for (var e = distSkip; e < elevations.length; e+=distSkip) {
            slopeToElevationIndex.push(e);
            var distance = e / elevations.length * totdistance / 1000;
            elediff = elevations[e].elevation - elevations[e-distSkip].elevation;
            slope = elediff / (distSkip * totdistance / elevations.length);
            c2.addRow([distance.toFixed(0), Math.round(slope * 100 * 10) / 10]);
        }
        slopeChart.draw(c2, {
            width: 700,
            height: 250,
            series: [{visibleInLegend: false}],
        })
}

function smooth(data) {
    if (data.length < 2) {
        return data;
    }

    var res = [];
    res.push(data[0]);
    for (var x = 1; x < data.length - 1; x++) {
        res.push((data[x-1] + data[x] + data[x+1]) / 3);
    }
    return res;
}

function getAscent(elevations) {
    var data = []
    for (var x = 0; x < elevations.length; x++) {
        data.push(elevations[x].elevation);
    }
    
    data = smooth(data);
    data = smooth(data);

    var ascent = 0;
    for (var x = 1; x < data.length; x++) {
        ascent += Math.max(data[x] - data[x-1], 0);
    }
    return ascent;
}

/**
 * @returns estimated time in hours
 */
function getEstimatedTime(dist_in_m, ascent_in_m) {
    var flat_time = dist_in_m / 1000 / CONFIG.SPEED_SETTINGS.SPEED_KM_FLAT;
    var ascent_time = ascent_in_m / CONFIG.SPEED_SETTINGS.ASCENT_METRES_MIN / 60;
    return flat_time + ascent_time;
}

function updateAscentInfo(elevations) {
    ELEMENTS.totalAscent.textContent = "Ascent: " +
        Math.round(getAscent(elevations)) + " m";
    var time = getEstimatedTime(totalDistance(), getAscent(elevations))
    var hours = Math.floor(time);
    var mins = Math.round(60 * (time - hours));
    ELEMENTS.estimatedTime.textContent =
        "Estimated time: " + hours + "h " + mins + "m";
}



function button(a, b, c) {
    this.name = a;
    this.id = c;
    this.action = b
}

function getCurrentState() {
    // by default use 6 digits
    var PRECISION = 6;

    var state = {
        markers: [],
        travelmodes: [],
        bounds: null,
    };

    for (var x = 0; x < sections.length; x++) {
        var marker = sections[x].marker;
        var pos = {
            lat: utils.round(marker.getPosition().lat(), PRECISION),
            lng: utils.round(marker.getPosition().lng(), PRECISION),
        };
        state.markers.push(pos);
        state.travelmodes.push(sections[x].travelMode);
    };

    // Note: map.getBounds() might return undefined
    // if the map is not fully loaded yet
    if (map.getBounds()) {
        state.bounds = {
            south: utils.round(map.getBounds().getSouthWest().lat(), PRECISION),
            north: utils.round(map.getBounds().getNorthEast().lat(), PRECISION),
            east: utils.round(map.getBounds().getNorthEast().lng(), PRECISION),
            west: utils.round(map.getBounds().getSouthWest().lng(), PRECISION),
        };
    };

    return state;
}

function getPermalink(state) {
    var serialized = JSON.stringify(state);
    serialized = serialized.replace(/"/g, "|");
    return location.origin + location.pathname + '#' + serialized;
}

function refreshPermalink() {
    ELEMENTS.permalinkEdit.value = getPermalink(getCurrentState());
}

function saveToHistory() {
    var serialized = JSON.stringify(getCurrentState());

    serialized = serialized.replace(/"/g, "|");
    location.hash = serialized; 
}

function setCurrentState(state) {
    clearMarkers();
    if (state == null) {
      refreshPermalink();
      return;
    }

    // resume markers
    if (state.markers) {
        for (var x = 0; x < state.markers.length; x++) {
            var lat = state.markers[x].lat;
            var lng = state.markers[x].lng;

            var latlng = new google.maps.LatLng(lat, lng);
            addMarker(latlng, sections.length);
        }
    }

    if (state.travelmodes) {
        utils.assert(state.travelmodes.length == state.markers.length);
        for (var x = 0; x < state.travelmodes.length; x++) {
            if (sections[x].travelMode != state.travelmodes[x]) {
                sections[x].travelMode = state.travelmodes[x];
                findDirections(x);
            }
        }
    }

    //resume viewport
    if (state.bounds) {
        var sw = new google.maps.LatLng(state.bounds.south, state.bounds.west);
        var ne = new google.maps.LatLng(state.bounds.north, state.bounds.east);
        map.fitBounds(new google.maps.LatLngBounds(sw, ne));
    }

    refreshPermalink();
    refreshDirections();
}

function loadFromHistory() {
    if (location.hash == "" || location.hash == "#") {
        // nothing to load
        return;
    }
    var serialized = location.hash.toString();
    serialized = serialized.substring(1);
    serialized = serialized.replace(/\|/g, "\"");
    var state = JSON.parse(serialized);
    if (state.load_from != undefined) {
        // we have the link to the state
        if (!state.load_from.match(/[a-zA-Z0-9._]+/)) {
            throw "Security violation!"
        }
        var request = new XMLHttpRequest()
        request.open("GET", state.load_from, true)
        request.onreadystatechange = function() {
            if (request.readyState == 4) {
                if (request.status == 200) {
                    var state = JSON.parse(request.responseText);
                    setCurrentState(state);
                } else {
                    alert('Could not load the trip!');
                }
            } else {
                // pass
            }
        }
        request.send(null);
    } else {
        // we have the state
        setCurrentState(state);
    }
}

function findNearestVertex(a) {
    var smDelta = 100;
    var closest = null;

    for (path = 0; path < sections.length - 1; path++) {
        var latLngs = sections[path].polyline.getPath();
        var verticies = latLngs.getArray();
        for (var i = 0; i < verticies.length - 1; i++) {
            var p1 = verticies[i];
            var p2 = verticies[i+1];
            var step = 10 / p1.distanceTo(p2); // each 10m
            // this interpolates between points (for straight lines)
            for (var t = 0; t < 1; t+= step) {
              var p = new google.maps.LatLng(p1.lat() * t + p2.lat() * (1-t),
                                             p1.lng() * t + p2.lng() * (1-t));
              var distance = a.distanceTo(p);
              if (distance < smDelta) {
                  smDelta = distance;
                  closest = [p, path];
              }
            }
        }
    }
    return closest;
}


var initComplete = 0;

function init() {
    initializeUI();
    initComplete = 1
    loadFromHistory();
    saved_trips.refreshOnPage();
    window.onhashchange = function () { loadFromHistory(); };
}

google.load("visualization", "1", {
    packages: ["corechart"]
});
google.setOnLoadCallback(init);

google.maps.LatLng.prototype.distanceTo = function(a){ 
  var e = Math, ra = e.PI/180; 
  var b = this.lat() * ra, c = a.lat() * ra, d = b - c; 
  var g = this.lng() * ra - a.lng() * ra;

  var tmp = e.pow(e.sin(d/2), 2) + e.cos(b) * e.cos(c) * e.pow(e.sin(g/2), 2);
  var f = 2 * e.asin(e.sqrt(tmp)); 
  return f * 6378.137 * 1000; 
}