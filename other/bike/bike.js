/*
 (c) original file from: http://www.ridefreebikemaps.com/
 (c) PPershing (heavy edits)
 */

var map;
var directionsService = new google.maps.DirectionsService;
var elevationService = new google.maps.ElevationService;
google.load("visualization", "1", {
    packages: ["columnchart"]
});

var sections = [];

var marker_container = [];
var ModeSpecificControls, mode_control, clckTimeOut = null,
    travelMode = google.maps.DirectionsTravelMode.DRIVING,
    results_container = [],
    polyline_container = [],
    directionsQueue = [],
    mousemarker = null,
    chart = null,
    al = null,
    al2 = null,
    divisionVerticies = [];

var CONFIG = {
    MAP_SETTINGS : {
        zoom: 8,
        center: new google.maps.LatLng(46.6, 7.7),
        mapTypeId: google.maps.MapTypeId.HYBRID
    },
};

var ELEMENTS = {
    travelModeSelect: document.getElementById("travel_mode_select"),
    mapCanvas: document.getElementById("map_canvas"),
    elevationChart: document.getElementById("elevation_chart"),
    slopeChart: document.getElementById("slope_chart"),
    totalDistance: document.getElementById("total_distance"),
    totalAscent: document.getElementById("total_ascent"),
}

function updateTravelMode() {
    switch (ELEMENTS.travelModeSelect.selectedIndex) {
        case 0: travelMode = google.maps.DirectionsTravelMode.WALKING
                break;
        case 1: travelMode = google.maps.DirectionsTravelMode.DRIVING
                break;
    }
}

function initializeUI() {
    map = new google.maps.Map(ELEMENTS.mapCanvas, CONFIG.MAP_SETTINGS);
    google.maps.event.addListener(map, "click", function (event) {addMarker(event.latLng);});

    mode_control = new MapControl(map, "top", ModeButtonCreator(), google.maps.ControlPosition.RIGHT);
    google.maps.event.addDomListener(ELEMENTS.travelModeSelect, "change", updateTravelMode);
    al = new rfbm.areaLine(ELEMENTS.elevationChart);
    al2 = new google.visualization.ColumnChart(ELEMENTS.slopeChart);
    updateTravelMode();
}

// FIXME: what is the effect of this formula on latlongs?
function findDistance(latLng1, latLng2) {
    return Math.sqrt(
        Math.pow(latLng1.lat() - latLng2.lat(), 2) + 
        Math.pow(latLng1.lng() - latLng2.lng(), 2)
    );
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
    for (marker_num = 0; marker_num < marker_container.length; marker_num++) {
        b.push([findDistance(c, marker_container[marker_num].getPosition()), marker_num]);
    }
    b.sort(function (a, b) {
        return a[0] - b[0]
    });
    var marker_index = b[0][1];
    polyline_container[marker_index] = new google.maps.Polyline(a);
    polyline_container[marker_index].distance = directions_result.routes[0].legs[0].distance.value / 1E3;
    results_container[marker_index] = directions_result;
    google.maps.event.addListener(polyline_container[marker_index], "click", function (a) {
        vertex = findSmallestDistance(a.latLng);
        addSplitMarker(vertex);
        divisionVerticies.push(vertex);
    });
    for (b = a = 0; b < polyline_container.length; b++) b in polyline_container && (a += polyline_container[b].distance);
    ELEMENTS.totalDistance.text = "Distance: " + Math.round(a) + " km";
    directionsQueue.length ? window.setTimeout(directionsQueueProcessor(), 1E3) : polyline_container.length == marker_container.length - 1 && initElevation()
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

function addMarker(position) {
    if (!position) return;

    var marker = new google.maps.Marker({
        position: position,
        map: map,
        draggable: true
    });
    section = {
        marker: marker,
        polyline: null,
        routing: null,
        routingMode: null,
    }
    sections.push(section);

    marker_container.push(marker);
    google.maps.event.addListener(marker, "click", function () {
        var b, c;
        var marker_index = marker_container.indexOf(marker);
        if (marker_index != 0 && marker_index < marker_container.length - 1) {
            b = marker_container[marker_index - 1]
            c = marker_container[marker_index + 1]
        }
        marker_container.splice(marker_index, 1);
        marker.setMap(null);
        if (polyline_container.length) {
            // Note: we erased 1 item!
            if (marker_index == marker_container.length) {
                polyline = polyline_container.splice(-1, 1);
                polyline[0].setMap(null);
                results_container.splice(-1, 1);
            } else {
                polyline = polyline_container.splice(marker_index, 1);
                polyline[0].setMap(null);
                results_container.splice(marker_index, 1); 
                if (marker_index != 0) {
                    polyline_container[marker_index - 1].setMap(null);
                    directionsQueueProcessor(b, c);
                }
            }
        }
    });
    google.maps.event.addListener(marker, "dragend", function () {
        if (marker_container.length > 0) {
            marker_index = marker_container.indexOf(marker);
            if (marker_index == 0) {
                (polyline = polyline_container.splice(0, 1), polyline[0].setMap(null), directionsQueueProcessor(marker_container[0], marker_container[marker_index + 1]))
            } else {
                if (marker_index == marker_container.length - 1) {
                    polyline = polyline_container.splice(-1, 1);
                    polyline[0].setMap(null);
                    directionsQueueProcessor(marker_container[marker_index - 1], marker_container[marker_index]);
                } else {
                    polyline_container[marker_index - 1].setMap(null);
                    polyline_container[marker_index].setMap(null);
                    directionsQueueProcessor(marker_container[marker_index - 1], marker_container[marker_index]);
                    directionsQueueProcessor(marker_container[marker_index], marker_container[marker_index + 1]);
                }
            }
        }
    });
    numMarkers = marker_container.length;
    numMarkers > 1 && (initComplete ? directionsQueueProcessor(marker_container[numMarkers - 2], marker_container[numMarkers - 1]) : directionsQueue.push([marker_container[numMarkers - 2], marker_container[numMarkers - 1]]))
}

function directionsQueueProcessor() {
    arguments.length && directionsQueue.push(arguments);
    directionsQueue.length > 0 ? (request = directionsQueue.shift()) ? findDirections(request[0], request[1]) : findDirections() : findDirections()
}

function findDirections() {
    window._fd += 1;
    if (arguments.length > 1) {
        start_point = arguments[0].getPosition();
        end_point = arguments[1].getPosition();
    } else {
        start_point = marker_container[marker_container.length - 2].getPosition();
        end_point = marker_container[marker_container.length - 1].getPosition();
    }
    directionsService.route({
        origin: start_point,
        destination: end_point,
        travelMode: travelMode,
        avoidHighways: true
    }, function (directions_result, directions_status) {
        if (directions_status == "OK") {
            directionsLoaded(directions_result);
        } else if (directions_status == "UNKNOWN_ERROR") {
            // FIXME: WTF is this???
            for (t = 0; polyline_container[t] != void 0 && t < polyline_container.length - 1; t++);
            marker_container[t] == void 0 ? directionsQueueProcessor(marker_container[t], marker_container[t + 1]) : directionsQueueProcessor(marker_container[marker_container.length - 2], marker_container[marker_container.length - 1])
        } else {
            // FIXME: really this is the only case?
            alert("The last waypoint could not be added to the map (bike routing outside US?.");
            alert(directions_status);
            m = marker_container.pop();
            m.setMap(null);
        }
    });
    google.maps.event.trigger(map, "resize")
}

function clearMarkers() {
    for (x = 0; x < marker_container.length; x++) {
        marker_container[x].setMap(null);
    }
    for (x = 0; x < polyline_container.length; x++) {
        polyline_container[x].setMap(null);
    }
    marker_container = [];
    polyline_container = [];
    results_container = [];
    divisionVerticies = [];
    al.clear();
    al2.clear();
}

function initElevation() {
    var a = [];
    for (path = 0; path < polyline_container.length; path++) {
        var latLngs = polyline_container[path].getPath();
        for (x = 0; x < latLngs.length; x++) {
            a.push(latLngs.getAt(x))
        }
    }
    if (a.length > 350) {
        _short = a.length / 260;
        old_verticies = a;
        a = [];
        for (x = 0; x <= old_verticies.length - 1; x += _short) {
            a.push(old_verticies[Math.round(x)])
        }
    }
    if (a.length > 0 && elevationService != null) {
        var query = {
            path: a,
            samples: 500
        }
        elevationService.getElevationAlongPath(query, plotElevation)
    } else {
        al.clear()
        al2.clear()
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
        newPosition = dragOnTrack(a.latLng);
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

function plotElevation(a, b) {
    if (b == google.maps.ElevationStatus.OK) {
        elevations = a;
        for (d = totdistance = 0; d < results_container.length; d++) totdistance += results_container[d].routes[0].legs[0].distance.value;
        var c = new google.visualization.DataTable;
        c.addColumn("number", "Distance");
        c.addColumn("number", "Elevation");
        for (var e = 0; e < a.length; e++) {
            elevations[e].distance = e / elevations.length * totdistance / 1000.0;
            c.addRow([elevations[e].distance, elevations[e].elevation]);
        }

        al.draw(c, {
            width: 611,
            height: 250,
            yTitle: "Elevation (m)",
            xTitle: "Distance (km)",
            mouseClick: function () {
                elevationClick();
            }
        })

        var c2 = new google.visualization.DataTable;
        c2.addColumn("string", "Dist");
        c2.addColumn("number", "Slope");
        distSkip = 1 + Math.floor(elevations.length * 1000 / totdistance);
        for (var e = distSkip; e < a.length; e+=distSkip) {
            elediff = elevations[e].elevation - elevations[e-distSkip].elevation;
            slope = elediff / (distSkip * totdistance / elevations.length);
            c2.addRow([Math.round(elevations[e].distance) + "", slope * 100]);
        }
        al2.draw(c2, {
            width: 811,
            height: 250,
            series: [{color: 'black', visibleInLegend: false}],
        })

        ascent = 0;
        for (var e = 1; e < a.length; e++) {
            diff = elevations[e].elevation - elevations[e-1].elevation;
            ascent += diff > 0 ? diff : 0
        }
        ELEMENTS.totalAscent.text = "Ascent: " + Math.round(ascent) + " m";
    } else initElevation()
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
            google.maps.event.addDomListener(b, "click", c[f].action);
        }
        this.setTopContainerStyle(g);
        break;
    case "drop":
        for (f = 0; f < c.length; f++) {
            h = document.createElement("div");
            h.appendChild(document.createTextNode(c[f].name));
            c[f].id && h.setAttribute("id", c[f].id), g.appendChild(h), this.setDropInnerStyle(h), google.maps.event.addDomListener(h, "click", c[f].action);
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

function directionsCenter(a) {
    var b = new google.maps.LatLngBounds;
    for (bound = 0; bound < a.length; bound++) b.extend(a[bound]);
    return b
}


function findSmallestDistance(a) {
    verticies = [];
    for (path = 0; path < polyline_container.length; path++) {
        latLngs = polyline_container[path].getPath();
        verticies = verticies.concat(latLngs.getArray());
    }
    smDelta = 100;
    smDelta.vertex = verticies[0];
    distance = 0;
    distances = [];
    for (v = 0; v < verticies.length; v++) {
        distance = findDistance(a, verticies[v]);
        distances.push(distance);
        if (distance < smDelta) {
            smDelta = distance;
            closestVertex = verticies[v];
        }
    }
    return closestVertex
}

function elevationClick() {
    var selection = al.getSelection();
    var vertex = findSmallestDistance(selection.location);
    addSplitMarker(vertex);
    divisionVerticies.push(vertex);
    return [vertex, v]
}

function dragOnTrack(a) {
    return findSmallestDistance(a)
};
var initComplete = !1;

function init() {
    if (!arguments.callee.done) {
        arguments.callee.done = !0;
        _timer && clearInterval(_timer);
        initializeUI();
        initComplete = !0
    }
}
document.addEventListener && document.addEventListener("DOMContentLoaded", init, !1);
if (/WebKit/i.test(navigator.userAgent)) var _timer = setInterval(function () {
    /loaded|complete/.test(document.readyState) && init()
}, 10);
window.onload = init;
var mousemarker = null;
