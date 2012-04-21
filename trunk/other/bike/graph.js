/*
 (c) original file from: http://www.ridefreebikemaps.com/
 (c) PPershing (heavy edits)
 */

var rfbm = {
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
    this.box.childNodes.length > 0 && this.clearChart();
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
rfbm.areaLine.prototype.clearChart = function () {
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
