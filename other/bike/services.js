Namespace('services.routing', {
 OSMRouteDirectionsService : function() {
    this.transport_url = "transport.php?url=";
    this.base_url = "http://www.yournavigation.org/api/dev/gosmore.php"

    this.route = function(query, callback) {
        var url = this.transport_url;
        url += this.base_url;
        url += "&flat=" + query.origin.lat();
        url += "&flon=" + query.origin.lng();
        url += "&tlat=" + query.destination.lat();
        url += "&tlon=" + query.destination.lng();
        url += "&v=" + query.travelMode;
        url += "&format=geojson";

        var request = new utils.JsonRpc();
        request.call(url, function(response, was_ok) {
            if (was_ok == "OK") {
                var lat_lngs = []
                for (var i = 1; i < response.coordinates.length; i++) {
                    lat_lngs.push(new
                    google.maps.LatLng(response.coordinates[i][1],
                    response.coordinates[i][0]));
                };

                result = {
                    steps: [{ lat_lngs: lat_lngs}],
                    distance: {
                        value: response.properties.distance * 1000,
                    }
                };
                callback(result, "OK");
            } else {
                callback(null, was_ok);
            }
        });
    };
 },

 GmapsRouteService : function() {
    this.service = new google.maps.DirectionsService();

    this.route = function(query, callback) {
        this.service.route(query,
            function (directions_result, directions_status) {
                if (directions_status == "OK") {
                    utils.assert(directions_result.routes.length >= 1);
                    utils.assert(directions_result.routes[0].legs.length == 1);
                    var result = directions_result.routes[0].legs[0];
                    callback(result, directions_status)
                } else {
                    callback(null, directions_status);
                }
            });
    }
 },

 StraightRouteService : function() {
    this.route = function(query, callback) {
        callback(this.getResult(query), "OK");
    };

    this.getResult = function(query) {
        var result = {
            steps: [{
                lat_lngs: [query.origin, query.destination],
            }],
            distance: {
                value: query.origin.distanceTo(query.destination),
            },
        };
        return result;
    };
 },

 CombinedRouteService : function() {
    this.openRouteService = new services.routing.OSMRouteDirectionsService();
    this.straightRouteService = new services.routing.StraightRouteService();
    this.googleRouteService = new services.routing.GmapsRouteService();

    this.subservices = {
        STRAIGHT : {
            type: null,
            engine : this.straightRouteService
        },
        WALKING : {
            type: google.maps.DirectionsTravelMode.WALKING,
            engine: this.googleRouteService
        },
        DRIVING : {
            type: google.maps.DirectionsTravelMode.DRIVING,
            engine: this.googleRouteService
        },
        OSM_WALK: {
            type : "foot",
            engine: this.openRouteService
        },
        OSM_BIKE: {
            type : "bicycle",
            engine: this.openRouteService
        },
    };

    this.queue = [];
    this.DELAY = 1000;
    this.timer = null;

    this.route = function(query, callback) {
        utils.assert(this.subservices[query.travelMode] != undefined, "Wrong travelmode" + query.travelMode);
        this.queue.push([query, callback]);
        if (this.timer == null) {
            this.timer = setInterval(this.processQueue.bind(this), this.DELAY);
        }
    };

    this.processQueue = function() {
        if (this.queue.length > 0) {
            var data = this.queue.splice(0, 1)[0];
            var query = data[0];
            var callback = data[1];

            var service = this.subservices[query.travelMode].engine;
            query.travelMode = this.subservices[query.travelMode].type;
            service.route(query, callback);
        } else {
            clearInterval(this.timer);
            this.timer = null;
        }
    };
 }
});


directionsService = new services.routing.CombinedRouteService();
directionsService.travelmodes = {
    STRAIGHT : {
        text: "Straight link",
    },
    WALKING : {
        text: "Walk (Gmaps)",
    },
    DRIVING : {
        text: "Car (Gmaps)",
    },
    OSM_WALK: {
        text : "Pedestian (OSM)",
    },
    OSM_BIKE : {
        text : "Bicycle (OSM)",
    }
};

Namespace('services.elevation', {
    ElevationService : function() {
        this.elevation = new google.maps.ElevationService(),
        this.queue = [];
        this.DELAY = 100;
        this.timer = null;
        this.getElevationAlongPath = function(request, callback) {
            this.queue.push([request, callback]);
            if (this.timer == null) {
                this.timer = setInterval(this.processQueue.bind(this), this.DELAY);
            }
        };
        this.processQueue = function() {
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
        };
    },
});

var elevationService = new services.elevation.ElevationService();

Namespace('services.geocoding', {
    GeocoderService : function() {
        this.geocoder = new google.maps.Geocoder();
        this.queue = [];
        this.DELAY = 2000;
        this.timer = null;
        this.geocode = function(request, callback) {
            this.queue.push([request, callback]);
            if (this.timer == null) {
                this.timer = setInterval(this.processQueue.bind(this), this.DELAY);
            }
        };
        this.processQueue = function() {
            if (this.queue.length > 0) {
                var data = this.queue.splice(0, 1)[0];
                var request = data[0];
                var callback = data[1];
                this.geocoder.geocode(request, callback);
            } else {
                clearInterval(this.timer);
                this.timer = null;
            }
        }
    },
});

var geocoderService = new services.geocoding.GeocoderService();

var rawSavedTracksService = {
    base_url : "tracks.php",

    getVisibleTracks: function(query, callback) {

        var url = this.base_url;
        url += "?query=get_visible_segments";
        url += "&west=" + query.bounds.getSouthWest().lng();
        url += "&north=" + query.bounds.getNorthEast().lat();
        url += "&east=" + query.bounds.getNorthEast().lng();
        url += "&south=" + query.bounds.getSouthWest().lat();
        url += "&tracks=" + query.tracks;

        var rpc = new utils.JsonRpc();
        rpc.call(url, function(response, was_ok) {
            if (was_ok == "OK") {
                callback(response);
            } else {
                console.log("Requesting tracks failed!");
                callback({});
            }
        });
    }
}

var savedTracksService = {
    service : rawSavedTracksService,
    queue : [],
    DELAY: 500,
    timer: null,
    getVisibleTracks: function(request, callback) {
        this.queue.push([request, callback]);
        if (this.timer == null) {
            this.processQueue();
            this.timer = setInterval(this.processQueue.bind(this), this.DELAY);
        }
    },
    processQueue: function() {
        if (this.queue.length > 0) {
            var data = this.queue[this.queue.length - 1];
            this.queue = [] 
            var request = data[0];
            var callback = data[1];
            this.service.getVisibleTracks(request, callback);
        } else {
            clearInterval(this.timer);
            this.timer = null;
        }
    },
};
