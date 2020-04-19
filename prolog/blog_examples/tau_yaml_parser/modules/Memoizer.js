export default (function() {

    var cache = {};

    function cacher(func) {
        return function() {
            var key = JSON.stringify(arguments);
            if (cache[key]) {
                return cache[key];
            } else{
                let val = func.apply(this, arguments);
                cache[key] = val;
                return val;
            }
        };
    }

    function cacherOnArg3(func) {
        return function(a, b, c) {
            var key = JSON.stringify(c);
            if (cache[key]) {
                return cache[key];
            } else{
                let val = func.apply(this, arguments);
                cache[key] = val;
                return val;
            }
        };
    }

    return {
        memo: function(func) {
            return cacher(func);
        },
        memo3: function(func) {
            return cacherOnArg3(func);
        }
    };

})();
