utils = {
  round : function(number, places) {
    var mult = Math.pow(10, places);
    return Math.round(number * mult) / mult;
  },
};
