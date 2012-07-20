utils = {
  /**
   * Rounds number to specified number of decimal places
   * @param float number
   * @param int places
   * @returns float
   */
  round : function(number, places) {
    var mult = Math.pow(10, places);
    return Math.round(number * mult) / mult;
  },

  /**
   * Wrapper which can store objects in simple string storage
   */
  ObjectStorage: function(stringStorage) {
    this.storage = stringStorage;

    /**
     * @param string key
     * @returns Object or null on error
     */
    this.getItem = function (key) {
      try {
        var serialized = this.storage.getItem(key);
        return JSON.parse(serialized);
      } catch (e) {
        return null;
      }
    }

    /**
     * @param string key
     * @param Object value
     */
    this.setItem = function(key, value) {
      var serialized = JSON.stringify(value);
      this.storage.setItem(key, serialized);
    }

    /**
     * @param string key
     */
    this.clear = function(key) {
      this.storage.clear(key);
    }
  },

  AssertException: function (message) {
      this.message = message;
  },

  assert: function(exp, message) {
      if (!exp) {
          throw new utils.AssertException(message);
      }
  }
};

utils.AssertException.prototype.toString = function () {
    return 'AssertException: ' + this.message;
}

utils.JsonRpc = function() {
    this.call = function(url, callback) {
        var request = new XMLHttpRequest();
        request.open("GET", url, true);
        request.onreadystatechange = function() {
            if (request.readyState == 4) {
                if (request.status == 200) {
                    var response = null;
                    var decode_ok = false;
                    try {
                        response = JSON.parse(request.responseText);
                        decode_ok = true;
                    } catch (err) {
                        decode_ok = false;
                    }

                    if (decode_ok) {
                        callback(response, "OK");
                    } else {
                        callback(null, "Request failed (invalid JSON)");
                    }
                } else {
                    callback(null, "Request failed (http" + request.status + ")");
                }
            } else {
                // pass
            }
        };
        request.send(null);
    }
}
