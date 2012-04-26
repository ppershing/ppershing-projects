utils = {
  round : function(number, places) {
    var mult = Math.pow(10, places);
    return Math.round(number * mult) / mult;
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
