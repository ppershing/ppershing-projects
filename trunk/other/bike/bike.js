/*
 (c) original file from: http://www.ridefreebikemaps.com/
 (c) PPershing (heavy edits)
 */

var map;
var directions = new google.maps.DirectionsService;
var elevation = new google.maps.ElevationService;
google.load("visualization", "1", {
    packages: ["columnchart"]
});
var directions_result;
var polygon_container = [];
var marker_container = [];
var ModeSpecificControls, mode_control, clckTimeOut = null,
    travelMode = google.maps.DirectionsTravelMode.DRIVING,
    results_container = [],
    polyline_container = [],
    directionsQue = [],
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
}

function initializeMap() {
    map = new google.maps.Map(ELEMENTS.mapCanvas, CONFIG.MAP_SETTINGS);
    google.maps.event.addListener(map, "click", mapClick);
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
    mode_control = new MapControl(map, "top", ModeButtonCreator(), google.maps.ControlPosition.RIGHT);
    google.maps.event.addDomListener(ELEMENTS.travelModeSelect, "change", updateTravelMode);
    al = new rfbm.areaLine(ELEMENTS.elevationChart);
    al2 = new google.visualization.ColumnChart(ELEMENTS.slopeChart);
    updateTravelMode();
}

function findDistance(latLng1, latLng2) {
    return Math.sqrt(
        Math.pow(latLng1.lat() - latLng2.lat(), 2) + 
        Math.pow(latLng1.lng() - latLng2.lng(), 2)
    );
}

function directionsLoaded() {
    var a = {
        path: directionsResultLatLngs(directions_result),
        strokeColor: "#0000CC",
        strokeOpacity: 0.4,
        map: map
    },
        b = [],
        c = a.path[0];
    for (marker_num = 0; marker_num < marker_container.length; marker_num++) b.push([findDistance(c, marker_container[marker_num].getPosition()), marker_num]);
    b.sort(function (a, b) {
        return a[0] - b[0]
    });
    polyline_container[b[0][1]] = new google.maps.Polyline(a);
    polyline_container[b[0][1]].distance = directions_result.routes[0].legs[0].distance.value / 1E3;
    results_container[b[0][1]] = directions_result;
    google.maps.event.addListener(polyline_container[b[0][1]], "click", function (a) {
        vertex = findSmallestDistance(a.latLng);
        addSplitMarker(vertex);
        divisionVerticies.push(vertex);
    });
    for (b = a = 0; b < polyline_container.length; b++) b in polyline_container && (a += polyline_container[b].distance);
    ELEMENTS.totalDistance.innerHTML = "Distance: " + Math.round(a) + " km";
    directionsQue.length ? window.setTimeout(directionsQueProcessor(), 1E3) : polyline_container.length == marker_container.length - 1 && initElevation()
}

function directionsResultLatLngs(a) {
    var b = [];
    for (x = 0; x < a.routes[0].legs[0].steps.length; x++) {
        for (y = 0; y < a.routes[0].legs[0].steps[x].lat_lngs.length - 1; y++) b.push(a.routes[0].legs[0].steps[x].lat_lngs[y]);
        b.push(a.routes[0].legs[0].steps[x].lat_lngs[y])
    }
    return b
}
function mapClick(a) {
    clckTimeOut ? (window.clearTimeout(clckTimeOut), clckTimeOut = null) : clckTimeOut = window.setTimeout(function () {
        addMarker(a.latLng)
    }, 275)
}

function addMarker(position) {
    var b, c;
    window.clearTimeout(clckTimeOut);
    clckTimeOut = null;
    if (!position) return;

    var marker = new google.maps.Marker({
        position: position,
        map: map,
        draggable: !0
    });
    marker_container.push(marker);
    google.maps.event.addListener(marker, "click", function () {
        var marker_index = marker_container.indexOf(marker);
        if (marker_index != 0 && marker_index < marker_container.length - 1) {
            b = marker_container[marker_index - 1]
            c = marker_container[marker_index + 1]
        }
        marker_container.splice(marker_index, 1);
        marker.setMap(null);
        if (polyline_container.length) {
            if (marker_index == marker_container.length) {
                polyline = polyline_container.splice(-1, 1);
                polyline[0].setMap(null);
                results_container.splice(-1, 1);
            } else {
                if (marker_index == 0) {
                    polyline = polyline_container.splice(marker_index, 1);
                    polyline[0].setMap(null);
                    results_container.splice(marker_index, 1); 
                } else {
                    polyline = polyline_container.splice(marker_index, 1);
                    polyline[0].setMap(null);
                    results_container.splice(marker_index, 1);
                    polyline_container[marker_index - 1].setMap(null);
                    directionsQueProcessor(b, c);
                }
            }
        }
    });
    google.maps.event.addListener(marker, "dragend", function () {
        if (marker_container.length > 0) {
            marker_index = marker_container.indexOf(marker);
            if (marker_index == 0) {
                (polyline = polyline_container.splice(0, 1), polyline[0].setMap(null), directionsQueProcessor(marker_container[0], marker_container[marker_index + 1]))
            } else {
                if (marker_index == marker_container.length - 1) {
                    polyline = polyline_container.splice(-1, 1);
                    polyline[0].setMap(null);
                    directionsQueProcessor(marker_container[marker_index - 1], marker_container[marker_index]);
                } else {
                    polyline_container[marker_index - 1].setMap(null);
                    polyline_container[marker_index].setMap(null);
                    directionsQueProcessor(marker_container[marker_index - 1], marker_container[marker_index]);
                    directionsQueProcessor(marker_container[marker_index], marker_container[marker_index + 1]);
                }
            }
        }
    });
    numMarkers = marker_container.length;
    numMarkers > 1 && (initComplete ? directionsQueProcessor(marker_container[numMarkers - 2], marker_container[numMarkers - 1]) : directionsQue.push([marker_container[numMarkers - 2], marker_container[numMarkers - 1]]))
}

function directionsQueProcessor() {
    arguments.length && directionsQue.push(arguments);
    directionsQue.length > 0 ? (request = directionsQue.shift()) ? findDirections(request[0], request[1]) : findDirections() : findDirections()
}

function findDirections() {
    window._fd += 1;
    arguments.length > 1 ? (start_point = arguments[0].getPosition(), end_point = arguments[1].getPosition()) : (start_point = marker_container[marker_container.length - 2].getPosition(), end_point = marker_container[marker_container.length - 1].getPosition());
    directions.route({
        origin: start_point,
        destination: end_point,
        travelMode: travelMode,
        avoidHighways: !0
    }, function (a, b) {
        if (b == "OK") {
            directions_result = a;
            directionsLoaded();
        } else if (b == "UNKNOWN_ERROR") {
            for (t = 0; polyline_container[t] != void 0 && t < polyline_container.length - 1; t++);
            marker_container[t] == void 0 ? directionsQueProcessor(marker_container[t], marker_container[t + 1]) : directionsQueProcessor(marker_container[marker_container.length - 2], marker_container[marker_container.length - 1])
        } else {
            alert("The last waypoint could not be added to the map.");
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
        latLngs = polyline_container[path].getPath();
        for (x = 0; x < latLngs.length; x++) {
            a.push(latLngs.getAt(x))
        }
    }
    if (a.length > 350) {
        _short = a.length / 260;
        old_verticies = a;
        a = [];
        for (x = 0; x < old_verticies.length; x += _short) {
            old_verticies[Math.round(x)] != void 0 && a.push(old_verticies[Math.round(x)])
        }
    }
    if (polyline_container.length > 0) {
        a = {
            path: a,
            samples: 500
        }
        elevation && elevation.getElevationAlongPath(a, plotElevation)
    } else {
        al.clear()
        al2.clear()
    }
}

function addSplitMarker(a) {
    splitMarker = new google.maps.Marker({
        position: a,
        map: map,
        draggable: !0,
        clickable: !0,
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
        document.getElementById("total_ascent").innerHTML = "Ascent: " + Math.round(ascent) + " m";
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
        for (var f = 0; f < c.length; f++) h = document.createElement("div"), h.appendChild(document.createTextNode(c[f].name)), c[f].id && h.setAttribute("id", c[f].id), b = document.createElement("div"), b.appendChild(h), g.appendChild(b), this.setTopInnerStyle(h), this.setTopMiddleStyle(b), google.maps.event.addDomListener(b, "click", c[f].action);
        this.setTopContainerStyle(g);
        break;
    case "drop":
        for (f = 0; f < c.length; f++) h = document.createElement("div"), h.appendChild(document.createTextNode(c[f].name)), c[f].id && h.setAttribute("id", c[f].id), g.appendChild(h), this.setDropInnerStyle(h), google.maps.event.addDomListener(h, "click", c[f].action);
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
    for (path = 0; path < polyline_container.length; path++) latLngs = polyline_container[path].getPath(), verticies = verticies.concat(latLngs.getArray());
    smDelta = 100;
    smDelta.vertex = verticies[0];
    distance = 0;
    distances = [];
    for (v = 0; v < verticies.length; v++) distance = findDistance(a, verticies[v]), distances.push(distance), distance < smDelta && (smDelta = distance, closestVertex = verticies[v]);
    return closestVertex
}

function elevationClick() {
    selection = al.getSelection();
    vertex = findSmallestDistance(selection.location);
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
        initializeMap();
        initializeUI();
        initComplete = !0
    }
}
document.addEventListener && document.addEventListener("DOMContentLoaded", init, !1);
if (/WebKit/i.test(navigator.userAgent)) var _timer = setInterval(function () {
    /loaded|complete/.test(document.readyState) && init()
}, 10);
window.onload = init;
var mousemarker = null,
    rfbm = {
        areaLine: function (a) {
            this.container = a;
            this.container.funcRef = this;
            this.box = document.createElement("div");
            this.box.style.width = "100%";
            this.container.appendChild(this.box);
            this.box.style.position = "relative"
        }
    };
rfbm.areaLine.prototype.draw = function (a, b) {
    this.box.childNodes.length > 0 && this.clear();
    var c = b.width ? b.width : this.container.parentNode.clientWidth;
    this.plotWidth = c - 100;
    var e = b.height ? b.height : this.container.parentNode.clientHeight;
    this.plotHeight = e - 100;
    var g = b.xTitle ? b.xTitle : null,
        h = b.yTitle ? b.yTitle : null;
    this.mouseOver = b.mouseOver ? b.mouseOver : function () {
        document.getElementById("selection") != null && document.getElementById("selection").parentNode.removeChild(this.selection);
        rfbm.selectCol(this);
        this.onmouseout = function () {
            this.removeChild(this.childNodes[1]);
            selectionMeter.childNodes[0].firstChild.data = "";
            selectionMeter.childNodes[1].firstChild.data = "";
            mousemarker != null && (mousemarker.setMap(null), mousemarker = null);
            this.onmouseout = null
        }
    };
    this.mouseClick = b.mouseClick ? b.mouseClick : null;
    this.box.style.width = c + "px";
    this.box.style.height = e + "px";
    dataBox = document.createElement("div");
    dataBox.setAttribute("id", "plot");
    dataBox.style.width = c - 100 + "px";
    dataBox.style.height = e - 100 + "px";
    dataBox.style.position = "relative";
    dataBox.style.left = "98px";
    var c = a.getNumberOfColumns(),
        f = a.getNumberOfRows();
    ymax = rfbm.getChartMax(a, 1);
    xmax = rfbm.getChartMax(a, 0);
    for (row = 0; row < f; row++) for (col = 1; col < c; col++) rect = document.createElement("div"), dataBox.appendChild(rect), rect.setAttribute("class", "line " + row), rect.onmouseover = this.mouseOver, rect.onclick = this.mouseClick, rect.style.width = this.plotWidth / (f * 4) + "%", rect.style.height = a.getValue(row, col) / ymax * 100 + "%", rect.x = a.getValue(row, 0), rect.y = a.getValue(row, col), rect.style.position = "absolute", rect.style.left = a.getValue(row, 0) / xmax * 100 + "%", rect.style.bottom = "0px", rect.style.display = "inline-block", rect.style.backgroundColor = "orange", rect.style.filter = "alpha(opacity=50)", rect.style.MozOpacity = "0.5", rect.style.khtmlOpacity = "0.5", rect.style.opacity = "0.5", transrect = document.createElement("div"), transrect.setAttribute("class", row), transrect.style.width = "100%", transrect.style.height = this.plotHeight + "px", transrect.style.position = "absolute", transrect.style.left = "0px", transrect.style.bottom = "0px", transrect.onmouseover = rect.mouseover, rect.appendChild(transrect);
    f = rfbm.getScale(a, 1);
    c = document.createElement("div");
    c.style.position = "absolute";
    c.style.right = this.plotWidth + "px";
    c.style.top = "0px";
    c.style.width = "100px";
    c.style.height = this.plotHeight + "px";
    for (n = 0; n < f.length; n++) div = document.createElement("div"), div.innerHTML = f[n], div.style.position = "absolute", div.style.bottom = (f[n] / rfbm.getChartMax(a, 1) - 11 / this.plotHeight) * 100 + "%", div.style.right = "0px", c.appendChild(div), line = document.createElement("div"), line.style.backgroundColor = "black", line.style.width = this.plotWidth + 4 + "px", line.style.height = "2px", line.style.position = "absolute", line.style.bottom = f[n] * 100 / rfbm.getChartMax(a, 1) + "%", line.style.left = "100px", line.style.filter = "alpha(opacity=10)", line.style.MozOpacity = "0.1", line.style.khtmlOpacity = "0.1", line.style.opacity = "0.1", c.appendChild(line);
    yTitleOut = document.createElement("div");
    yTitleOut.style.position = "absolute";
    yTitleOut.style.left = "-44px";
    yTitleOut.style.top = (0.5 - 22 / e * 0.5) * 100 + "%";
    yTitleOut.style.width = this.plotHeight + "px";
    yTitleOut.style.height = "22px";
    yTitleIn = document.createElement("div");
    yTitleIn.setAttribute("id", "ytitle");
    yTitleIn.innerHTML = h;
    yTitleIn.style.writingMode = "tb-rl";
    yTitleIn.style.webkitTransform = "rotate(90deg)";
    yTitleIn.style.MozTransform = "rotate(90deg)";
    yTitleIn.style.textAlign = "center";
    yTitleIn.style.width = "100%";
    yTitleIn.style.height = "100%";
    yTitleOut.appendChild(yTitleIn);
    c.appendChild(yTitleOut);
    h = rfbm.getScale(a, 0);
    f = document.createElement("div");
    f.style.position = "absolute";
    f.style.right = "0px";
    f.style.top = this.plotHeight + "px";
    f.style.width = this.plotWidth + "px";
    f.style.height = "25px";
    for (n = 0; n < h.length; n++) div = document.createElement("div"), div.innerHTML = h[n], div.style.position = "absolute", div.style.bottom = "0px", curLen = h[n].toString().length * 7, div.style.left = (h[n] / rfbm.getChartMax(a, 0) - 0.5 * curLen / this.plotWidth) * 100 + "%", f.appendChild(div), line = document.createElement("div"), line.style.backgroundColor = "black", line.style.width = "2px", line.style.height = this.plotHeight, line.style.position = "absolute", line.style.left = (h[n] / rfbm.getChartMax(a, 0) - 2 / this.plotWidth) * 100 + "%", line.style.bottom = "22px", line.style.filter = "alpha(opacity=20)", line.style.MozOpacity = "0.2", line.style.khtmlOpacity = "0.2", line.style.opacity = "0.2", f.appendChild(line);
    xTitleDiv = document.createElement("div");
    xTitleDiv.setAttribute("id", "xtitle");
    xTitleDiv.innerHTML = g;
    xTitleDiv.style.position = "absolute";
    xTitleDiv.style.width = "100%";
    xTitleDiv.style.height = "22px";
    xTitleDiv.style.top = "22px";
    xTitleDiv.style.right = "0px";
    xTitleDiv.style.textAlign = "center";
    f.appendChild(xTitleDiv);
    this.selectionMeter = document.createElement("DIV");
    this.selectionMeter.setAttribute("id", "selectionMeter");
    this.selectionMeter.appendChild(document.createElement("div"));
    this.selectionMeter.appendChild(document.createElement("div"));
    this.selectionMeter.childNodes[0].style.width = "50%";
    this.selectionMeter.childNodes[0].style.height = "100%";
    this.selectionMeter.childNodes[0].style.display = "inline-block";
    this.selectionMeter.childNodes[0].appendChild(document.createTextNode(" "));
    this.selectionMeter.childNodes[1].style.width = "50%";
    this.selectionMeter.childNodes[1].style.height = "100%";
    this.selectionMeter.childNodes[1].style.display = "inline-block";
    this.selectionMeter.childNodes[1].appendChild(document.createTextNode(" "));
    this.selectionMeter.style.position = "absolute";
    this.selectionMeter.style.height = "22px";
    this.selectionMeter.style.width = "400px";
    this.selectionMeter.style.top = e - 44 + "px";
    this.selectionMeter.style.right = "0px";
    dataBox.appendChild(this.selectionMeter);
    this.box.appendChild(dataBox);
    dataBox.appendChild(f);
    dataBox.appendChild(c)
};
rfbm.areaLine.prototype.selection = null;
rfbm.areaLine.prototype.getSelection = function () {
    return document.getElementById("selection")
};
rfbm.areaLine.prototype.setSelection = function (a) {
    document.getElementById("selection") != null && document.getElementById("selection").parentNode.removeChild(this.selection);
    return this.selection = rfbm.selectCol(a)
};
rfbm.areaLine.prototype.clear = function () {
    this.box.removeChild(this.box.firstChild)
};
rfbm.getChartMax = function (a, b) {
    return a.getColumnRange(b).max
};
rfbm.getScale = function (a, b) {
    var c = rfbm.getChartMax(a, b),
        e = rfbm.roundBaseTen(c),
        g = [],
        c = rfbm.roundBaseTen(rfbm.roundBaseTen(c) / 4) == rfbm.roundBaseTen(c) / 4 ? rfbm.roundBaseTen(c) / 4 : rfbm.roundBaseTen(c) / 5;
    for (i = 0; i <= e; i += c) g.push(i);
    return g
};
rfbm.roundBaseTen = function (a) {
    a = Math.round(a);
    return rounded = Math.round(a / Math.pow(10, a.toString().length - 1)) * Math.pow(10, a.toString().length - 1)
};
rfbm.parseStyle = function (a) {
    length = a.length;
    a[length - 1] == "%" ? num = parseFloat(a.slice(0, length - 2)) : a[length - 1] == "x" && (num = parseFloat(a.slice(0, length - 3)));
    return num
};
rfbm.selectCol = function (a) {
    if (typeof a == "number") plot = document.getElementById("plot"), a = plot.childNodes[a];
    else if (typeof a == "object") if (a.type == "mouseover") a = this, plot = a.parentNode;
    else if (a.tagName == "DIV") plot = a.parentNode;
    colWidth = rfbm.parseStyle(plot.style.width);
    colLeft = rfbm.parseStyle(plot.style.left);
    plotWidth = rfbm.parseStyle(plot.parentNode.style.width);
    selection = document.createElement("div");
    selection.style.height = "100%";
    selection.style.width = "200%";
    selection.setAttribute("id", "selection");
    selection.style.backgroundColor = "green";
    selection.style.zIndex = 1;
    selection.x = a.x;
    selection.y = a.y;
    selection.location = elevations[a.className.slice(5)].location;
    a.appendChild(selection);

    if (mousemarker == null) {
        mousemarker = new google.maps.Marker({
            map: map,
            icon: "http://maps.google.com/mapfiles/ms/icons/green-dot.png"
        });
    }
    mousemarker.setPosition(elevations[a.className.slice(5)].location);

    rfbm.showSelectionValues(selection);
    return selection
};

rfbm.showSelectionValues = function (a) {
    selectionMeter = document.getElementById("selectionMeter");
    ytitle = document.getElementById("ytitle").firstChild.data;
    xtitle = document.getElementById("xtitle").firstChild.data;
    selectionMeter.childNodes[0].firstChild.data = "Distance " + Math.round(a.x * 10) / 10 + " km";
    selectionMeter.childNodes[1].firstChild.data = "Height " + Math.round(a.y) + " m"
};
