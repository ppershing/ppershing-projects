var saved_trips = {
    SAVED_TRIPS_KEY : "saved_trips",

    storage: new utils.ObjectStorage(window.localStorage),

    rootElement: ELEMENTS.savedList,

    init: function() {
        if (this.storage.getItem(this.SAVED_TRIPS_KEY) == null) {
            this.storage.setItem(this.SAVED_TRIPS_KEY, []);
        };
    },

    remove: function(id) {
        var data = this.storage.getItem(this.SAVED_TRIPS_KEY);
        data.splice(id,1);
        this.storage.setItem(this.SAVED_TRIPS_KEY, data);
        this.refreshOnPage();
    },

    add: function(trip_data) {
        var data = this.storage.getItem(this.SAVED_TRIPS_KEY);
        data.push(trip_data);
        this.storage.setItem(this.SAVED_TRIPS_KEY, data);
        this.refreshOnPage();
    },

    refreshOnPage: function() {
        var rootElement = this.rootElement;

        // Beware, indexes are shifting!
        while (rootElement.children.length) {
          rootElement.removeChild(rootElement.children[0]);
        }

        var results = this.storage.getItem(this.SAVED_TRIPS_KEY);
        var len = results.length;
        // Seems strange to use one more layer of functions?
        // Then refer to
        // http://perplexed.co.uk/559_javascript_lambda_functions_and_closures.htm
        var delete_onclick = function(id) {
            return function() {
                this.remove(id);
                this.refreshOnPage();
            }.bind(this);
        }.bind(this);
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
};

saved_trips.init();

