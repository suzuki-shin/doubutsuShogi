Elm.Native.ListProc = {};

Elm.Native.ListProc.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.ListProc = elm.Native.ListProc || {};
    if (elm.Native.ListProc.values) return elm.Native.ListProc.values;

    /* ここに実装を書く */
    function log (x) {
        console.log(x);
        return x;
    }

    // 別のモジュールを使用する時はこの様にする。
    // Elm.Native.UtilsにはElmの型を構築する関数などが入っている。
    var Utils = Elm.Native.Utils.make(elm);
    function takeWhileString(p, l) {
        for(var i = 0; i < l.length; i++) {
            // Utils.chrを用いてChar型にする。
            // 1引数関数にはそのまま引数を渡せばOK
            // 2引数以上ならA2(f, a1, a2)の様にA?関数に渡す。
            if(!p(Utils.chr(l[i]))) {
                return l.slice(0,i);
            }
        }
        return l;
    }

    return elm.Native.ListProc.values = {
        /* ここにエクスポートする関数を列挙する */
        log: log,
        takeWhileString: F2(takeWhileString), // 2引数以上の時はF?関数で包む。
    };
};
