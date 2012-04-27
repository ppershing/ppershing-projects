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
