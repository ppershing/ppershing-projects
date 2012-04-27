// vim: set sts=4: ts=4: sw=4: et
/*
 (c) original file from: http://www.ridefreebikemaps.com/
 (c) PPershing (heavy edits)
 */

var map;
var directionsService = new google.maps.DirectionsService;
var elevationService = new google.maps.ElevationService;
var geocoderService = new google.maps.Geocoder;

google.load("visualization", "1", {
    packages: ["corechart"]
});
var storage = new utils.ObjectStorage(window.localStorage);

var travelMode = google.maps.DirectionsTravelMode.DRIVING,
    mousemarker = null,
    elevationChart = null,
    slopeChart = null,
    divisionVerticies = [];

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
    /** storage keys */
    STORAGE : {
      SAVED_TRIPS : "saved_trips",
    }
};

var ELEMENTS = {
    travelModeSelect: document.getElementById("travel_mode_select"),
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
}

function updateTravelMode() {
    switch (ELEMENTS.travelModeSelect.selectedIndex) {
        case 0: travelMode = google.maps.DirectionsTravelMode.WALKING
                break;
        case 1: travelMode = google.maps.DirectionsTravelMode.DRIVING
                break;
    }
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
      var select = createModeSelector("");
      label.textContent = name;

      entry.appendChild(label);
      entry.appendChild(select);
      rootElement.appendChild(entry);
    };
}

function initializeUI() {
    map = new google.maps.Map(ELEMENTS.mapCanvas, CONFIG.MAP_SETTINGS);
    google.maps.event.addListener(map, "click", function (event) {addMarker(event.latLng);});
    google.maps.event.addListener(map, "bounds_changed", function (event) {refreshPermalink()});

    // "skip"
    new MapControl(map, "top", ModeButtonCreator(), google.maps.ControlPosition.RIGHT);
    ELEMENTS.travelModeSelect.addEventListener("change", updateTravelMode);
    ELEMENTS.saveTripButton.addEventListener("click", 
        function() {addSavedTrip({name: ELEMENTS.saveTripTextName.value, data:getCurrentState()});}
      );

    elevationChart = new google.visualization.AreaChart(ELEMENTS.elevationChart);
    google.visualization.events.addListener(elevationChart, 'onmouseover', elevationMouseOver);
    google.visualization.events.addListener(elevationChart, 'onmouseout', elevationMouseOut);
    google.visualization.events.addListener(elevationChart, 'select', elevationClick);
    plotElevation([]);

    slopeChart = new google.visualization.ColumnChart(ELEMENTS.slopeChart);
    google.visualization.events.addListener(slopeChart, 'onmouseover', slopeMouseOver);
    google.visualization.events.addListener(slopeChart, 'onmouseout', slopeMouseOut);
    plotSlope([]);

    updateTravelMode();
}

// FIXME: what is the effect of this formula on latlongs?
function findDistance(latLng1, latLng2) {
    return Math.sqrt(
        Math.pow(latLng1.lat() - latLng2.lat(), 2) + 
        Math.pow(latLng1.lng() - latLng2.lng(), 2)
    );
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
    var vertex = findNearestVertex(elevations[elevationChartLastPos].location);
    addSplitMarker(vertex);
    divisionVerticies.push(vertex);
    return [vertex, v]
}

function slopeMouseOut(e) {
    elevationChart.setSelection([])
}

function directionsLoaded(directions_result) {
    var a = {
        path: directionsResultLatLngs(directions_result),
        strokeColor: "#0000CC",
        strokeOpacity: 0.4,
        map: map
    },
        b = [],
        c = a.path[0];
    for (marker_num = 0; marker_num < sections.length; marker_num++) {
        b.push([findDistance(c, sections[marker_num].marker.getPosition()), marker_num]);
    }
    b.sort(function (a, b) {
        return a[0] - b[0]
    });
    var marker_index = b[0][1];
    sections[marker_index].polyline = new google.maps.Polyline(a);
    sections[marker_index].polyline.distance = directions_result.routes[0].legs[0].distance.value / 1E3;
    sections[marker_index].results = directions_result;

    google.maps.event.addListener(sections[marker_index].polyline, "click", function (a) {
        vertex = findNearestVertex(a.latLng);
        addSplitMarker(vertex);
        divisionVerticies.push(vertex);
    });
    var a = 0;
    for (var x = 0; x < sections.length; x++) {
        a += sections[x].polyline.distance;
    }
    ELEMENTS.totalDistance.textContent = "Distance: " + Math.round(a) + " km";
    updateElevation()
}

function directionsResultLatLngs(a) {
    var b = [];

    for (x = 0; x < a.routes[0].legs[0].steps.length; x++) {
        for (y = 0; y < a.routes[0].legs[0].steps[x].lat_lngs.length - 1; y++) {
            b.push(a.routes[0].legs[0].steps[x].lat_lngs[y]);
        }
        b.push(a.routes[0].legs[0].steps[x].lat_lngs[y])
    }
    return b
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
    if (marker_index != sections.length && marker_index != 0) {
        sections[marker_index - 1].polyline.setMap(null);
        findDirections(marker_index - 1);
    }
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
        
        sections[marker_index - 1].polyline.setMap(null);
        sections[marker_index].polyline.setMap(null);
        findDirections(marker_index - 1);
        findDirections(marker_index);
    }

    refreshPermalink();
    geocode(marker_index);
}


function addMarker(position) {
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
        geocode: null,
        results: null,
    };

    sections.push(section);

    if (sections.length > 1) {
        findDirections(sections.length - 2);
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
            // stale response, ignore
            return;
        }
        if (status == "OK") {
            sections[index].geocode = results[0].formatted_address;
        } else {
            sections[index].geocode = null;
        }
        refreshDirections();
      });
}

function createModeSelector(selected_mode) {
  var select = document.createElement("select");
  options = [ ["line", "Straight link"],
              ["walk", "Walk"],
              ["car", "Car"],
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
    utils.assert(x >= 0 && x < sections.length);
    utils.assert(y >= 0 && y < sections.length);

    var start_point = sections[x].marker.getPosition();
    var end_point = sections[y].marker.getPosition();
    directionsService.route({
        origin: start_point,
        destination: end_point,
        travelMode: travelMode,
        avoidHighways: true
    }, function (directions_result, directions_status) {
        if (x >= sections.length ||
            y >= sections.length || 
            start_point != sections[x].marker.getPosition() ||
            end_point != sections[y].marker.getPosition()) {
          // this is a stale callback, skip the results! 
          return;
        }
        if (directions_status == "OK") {
            directionsLoaded(directions_result);
        } else if (directions_status == "UNKNOWN_ERROR") {
            // FIXME: What to do?
        } else {
            // FIXME: really this is the only case?
            alert("The last waypoint could not be added to the map (bike routing outside US?.");
            alert(directions_status);
        }
    });
    google.maps.event.trigger(map, "resize")
}

function clearMarkers() {
    for (x = 0; x < sections.length; x++) {
        sections[x].marker.setMap(null);
        sections[x].polyline.setMap(null);
    }
    sections = [];
    divisionVerticies = [];
    // Due to some unknown reason it might be null even after initialization
    elevationChart.clearChart();
    slopeChart.clearChart();
    refreshPermalink();
}

function updateElevation() {
    var track = [];
    for (var segment = 0; segment < sections.length; segment++) {
        var latLngs = sections[segment].polyline.getPath();
        for (var x = 0; x < latLngs.length; x++) {
            track.push(latLngs.getAt(x))
        }
    }

    if (track.length > 350) {
        var skip = track.length / 340;
        var old = track;
        track = [];
        for (var x = 0; x <= old.length - 1; x += skip) {
            // better to use floor than round (will not round to same value
            // twice
            track.push(old[Math.floor(x)])
        }
    }

    if (track.length > 0 && elevationService != null) {
        var query = {
            path: track,
            samples: 500
        }
        elevationService.getElevationAlongPath(query, plotElevationCallback)
    } else {
        ELEMENTS.totalAscent.textContent = "total ascent: N/A"
        elevationChart.clearChart()
        slopeChart.clearChart()
    }
}

function addSplitMarker(a) {
    splitMarker = new google.maps.Marker({
        position: a,
        map: map,
        draggable: true,
        clickable: true,
        oldPosition: a,
        icon: "http://labs.google.com/ridefinder/images/mm_20_white.png"
    });
    google.maps.event.addListener(splitMarker, "drag", function (a) {
        oldPosition = this.oldPosition;
        newPosition = findNearestVertex(a.latLng);
        this.setPosition(newPosition);
        vertexIndex = divisionVerticies.indexOf(oldPosition);
        this.oldPosition = divisionVerticies[vertexIndex] = newPosition
    });
    google.maps.event.addListener(splitMarker, "dragend", function () {
    });
    google.maps.event.addListener(splitMarker, "click", function () {
        i = divisionVerticies.indexOf(this.getPosition());
        divisionVerticies.splice(i, 1);
        this.setMap(null);
    });
    return splitMarker
}

function totalDistance() {
    for (d = totdistance = 0; d < sections.length; d++) {
        if (sections[d].results) totdistance += sections[d].results.routes[0].legs[0].distance.value;
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
            width: 800,
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
            width: 800,
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

function plotElevationCallback(a, b) {
    if (b == google.maps.ElevationStatus.OK) {
        // FIXME: this needs to be global due to the stupid graph!
        elevations = a;
        plotElevation(elevations)
        plotSlope(elevations)
        updateAscentInfo(elevations)
    } else {
        elevationChart.clearChart();
        slopeChart.clearChart();
        ELEMENTS.totalAscent.textContent = "error retrieving elevation info!";
    }
}


function button(a, b, c) {
    this.name = a;
    this.id = c;
    this.action = b
}

function MapControl(a, b, c, e) {
    var g = document.createElement("div"),
        h;
    switch (b) {
    case "top":
        for (var f = 0; f < c.length; f++) {
            h = document.createElement("div");
            h.appendChild(document.createTextNode(c[f].name));
            c[f].id && h.setAttribute("id", c[f].id);
            b = document.createElement("div");
            b.appendChild(h);
            g.appendChild(b);
            this.setTopInnerStyle(h);
            this.setTopMiddleStyle(b);
            b.addEventListener("click", c[f].action);
        }
        this.setTopContainerStyle(g);
        break;
    case "drop":
        for (f = 0; f < c.length; f++) {
            h = document.createElement("div");
            h.appendChild(document.createTextNode(c[f].name));
            if (c[f].id) {
              h.setAttribute("id", c[f].id);
            };
            g.appendChild(h);
            this.setDropInnerStyle(h);
            h.addEventListener("click", c[f].action);
        }
        this.setDropContainerStyle(g)
    }
    g.index = 1;
    a.controls[e].push(g);
    return g
}
MapControl.prototype.setTopInnerStyle = function (a) {
    a.style.border = "1px solid";
    a.style.borderColor = "white rgb(176, 176, 176) rgb(176, 176, 176) white";
    a.style.font = "12px Arial";
    a.style.width = "63px"
};
MapControl.prototype.setTopMiddleStyle = function (a) {
    a.style.border = "1px solid black";
    a.style.cursor = "pointer";
    a.style.textAlign = "center";
    a.style.display = "inline-block"
};
MapControl.prototype.setTopContainerStyle = function (a) {
    a.style.backgroundColor = "white";
    a.style.color = "black";
    a.style.height = "18px";
    a.style.marginRight = "5px";
    a.style.marginTop = "5px";
    a.style.lineHeight = "15px";
    a.className = "dont_pri"
};
MapControl.prototype.setDropContainerStyle = function (a) {
    a.style.color = "black";
    a.style.backgroundColor = "white";
    a.style.border = "1px solid black";
    a.className = "dont_pri"
};
MapControl.prototype.setDropInnerStyle = function (a) {
    a.style.font = "12px Arial";
    a.style.width = "59px";
    a.style.cursor = "pointer";
    a.style.paddingLeft = "6px"
};

function ModeButtonCreator() {
    var a = [];
    a.push(new button("Start Over", function () {
        clearMarkers()
    }));
    return a
}

function getCurrentState() {
    // by default use 6 digits
    var PRECISION = 6;

    var state = {
        markers: [],
        bounds: null,
    };

    for (var x = 0; x < sections.length; x++) {
        var marker = sections[x].marker;
        var pos = {
            lat: utils.round(marker.getPosition().lat(), PRECISION),
            lng: utils.round(marker.getPosition().lng(), PRECISION),
        };
        state.markers.push(pos);
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
            addMarker(latlng);
        }
    }

    //resume viewport
    if (state.bounds) {
        var sw = new google.maps.LatLng(state.bounds.south, state.bounds.west);
        var ne = new google.maps.LatLng(state.bounds.north, state.bounds.east);
        map.fitBounds(new google.maps.LatLngBounds(sw, ne));
    }

    refreshPermalink();
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
    setCurrentState(state);
}

function directionsCenter(a) {
    var b = new google.maps.LatLngBounds;
    for (bound = 0; bound < a.length; bound++) b.extend(a[bound]);
    return b
}


function findNearestVertex(a) {
    var verticies = [];
    for (path = 0; path < sections.length; path++) {
        var latLngs = sections[path].polyline.getPath();
        verticies = verticies.concat(latLngs.getArray());
    }
    var smDelta = 100;
    var distance = 0;
    var closestVertex = null;
    for (v = 0; v < verticies.length; v++) {
        distance = findDistance(a, verticies[v]);
        if (distance < smDelta) {
            smDelta = distance;
            closestVertex = verticies[v];
        }
    }
    return closestVertex
}


var initComplete = !1;

function init() {
    if (!arguments.callee.done) {
        arguments.callee.done = !0;
        if (_timer) {
          clearInterval(_timer);
        };

        if (storage.getItem(CONFIG.STORAGE.SAVED_TRIPS) == null) {
          storage.setItem(CONFIG.STORAGE.SAVED_TRIPS, []);
        }

        initializeUI();
        initComplete = 1
        loadFromHistory();
        refreshSavedTrips();
        window.onhashchange = function () { loadFromHistory(); };
    }
}

function removeSavedTrip(id) {
  var data = storage.getItem(CONFIG.STORAGE.SAVED_TRIPS);
  data.splice(id,1);
  storage.setItem(CONFIG.STORAGE.SAVED_TRIPS, data);
  refreshSavedTrips();
}


function addSavedTrip(trip_data) {
  var data = storage.getItem(CONFIG.STORAGE.SAVED_TRIPS);
  data.push(trip_data);
  storage.setItem(CONFIG.STORAGE.SAVED_TRIPS, data);
  refreshSavedTrips();
}

function refreshSavedTrips() {
    var rootElement = ELEMENTS.savedList;

    // Beware, indexes are shifting!
    while (rootElement.children.length) {
      rootElement.removeChild(rootElement.children[0]);
    }

    var results = storage.getItem(CONFIG.STORAGE.SAVED_TRIPS);
    var len = results.length;
    // Seems strange to use one more layer of functions?
    // Then refer to
    // http://perplexed.co.uk/559_javascript_lambda_functions_and_closures.htm
    var delete_onclick = function(id) {
      return function() {
        removeSavedTrip(id);
        refreshSavedTrips();
      };
    };
    for (var i = 0; i < len; ++i) {
      var row = results[i];
      // the entire entry
      var entry = document.createElement("div");
      var delete_button = document.createElement("input");
      var link = document.createElement("a");

      delete_button.type = "button";
      delete_button.value = "X";
      delete_button.onclick = delete_onclick(i);

      link.href = getPermalink(results[i].data);
      link.textContent = row.name;

      entry.appendChild(delete_button);
      entry.appendChild(link);
      rootElement.appendChild(entry);
    };
}
document.addEventListener && document.addEventListener("DOMContentLoaded", init, !1);
if (/WebKit/i.test(navigator.userAgent)) var _timer = setInterval(function () {
    /loaded|complete/.test(document.readyState) && init()
}, 10);
window.onload = init;
