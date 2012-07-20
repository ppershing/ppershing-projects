var server_trips = {
    TRIP_FILE : "trips/trips",
    rootElement: ELEMENTS.serverTripsList,

    init: function() {
        var rpc = new utils.JsonRpc();
        rpc.call(this.TRIP_FILE, function(response, was_ok) {
            if (was_ok == "OK") {
                server_trips.refreshOnPage(response);
            } else {
                server_trips.refreshOnPage([]);
            }
        });
    },

    refreshOnPage: function(trips) {
        var rootElement = this.rootElement;

        // Beware, indexes are shifting!
        while (rootElement.children.length) {
          rootElement.removeChild(rootElement.children[0]);
        }

        var len = trips.length;
        for (var i = 0; i < len; ++i) {
            // the entire entry
            var entry = document.createElement("div");
            var link = document.createElement("a");

            // TODO: sanitize trips[i].file
            link.href = "#{|load_from|:|" + trips[i].file + "|}";
            link.textContent = trips[i].desc;

            entry.appendChild(link);
            rootElement.appendChild(entry);
        };
    }

};

server_trips.init();

