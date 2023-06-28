
function mkTextW(v, i) {
    var w = { value: i
            , get: function() { return this.value; }
            , put: function(x) {
                    this.value = x;
                    this.view && $(this.view).html(this.value);
                }
            , view: null
            , setView: function(v2) {
                    this.view = v2;
                    this.view && $(this.view).html(this.value);
                }
            };
    w.setView(v);
    return w;
}

function mkButtonW(view, clickf) {
    var w = { view: null
            , setView: function(v2) {
                    this.view = v2;
                    this.view && $(this.view).on('click', clickf);
                }
            };
    w.setView(view);
    return w;
}

function mkRangeW(view, inputf) {
    var w = { put: function(x) {
                    this.view && $(this.view).val(x)
                }
            , view: null
            , setView: function(v2) {
                    this.view = v2;
                    this.view && $(this.view).on('input',
                        function() { inputf(this.value); }
                    );
                }
            };
    w.setView(view);
    return w;
}

