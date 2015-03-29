Elm.Native.Milkcocoa = {};

Elm.Native.Milkcocoa.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Milkcocoa = elm.Native.Milkcocoa || {};
    if (elm.Native.Milkcocoa.values) return elm.Native.Milkcocoa.values;

    var Signal = Elm.Signal.make(elm);

    /* ここに実装を書く */
    function dataStore(appid, path, sess) {
        var mc = new MilkCocoa("https://"+ appid +".mlkcca.com");
        console.log(path);
        console.log(mc);
        var ds = mc.dataStore(path).child(sess);

//         function on(event, callback) {
//             ds.on(event, callback);
//         }
        function push(obj) {
            ds.push(obj);
        }

        return A3(Signal.map 
    }

    return elm.Native.Milkcocoa.values = {
        /* ここにエクスポートする関数を列挙する */
//         milkcocoa: milkcocoa,
        dataStore: F3(dataStore),
//         push: push,
//         child: child,
//         on: on,
//         push: push,
    };
};
