
function init() {

    // ------- Event handlers --------

    function mousedownEvent(viewXMax, viewYMax, unitSize, e) {
        e.preventDefault();
        var x = (e.offsetX - viewXMax) / unitSize;
        var y = (viewYMax - e.offsetY) / unitSize;
        pushEvent("MouseDown (" + x + "," + y + ")");
    }

    function mouseupEvent(viewXMax, viewYMax, unitSize, e) {
        e.preventDefault();
        var x = (e.offsetX - viewXMax) / unitSize;
        var y = (viewYMax - e.offsetY) / unitSize;
        pushEvent("MouseUp (" + x + "," + y + ")");
    }

    function mousemoveEvent(viewXMax, viewYMax, unitSize, e) {
        e.preventDefault();
        var x = (e.offsetX - viewXMax) / unitSize;
        var y = (viewYMax - e.offsetY) / unitSize;
        pushEvent("MouseMove (" + x + "," + y + ")");
    }

    var pressedKeys = [];

    function keydownEvent(e) {
        e.preventDefault();
        var isRepeating = !!pressedKeys[e.keyCode];
        pressedKeys[e.keyCode] = true;
        if (!isRepeating) {
            var name = e.key.toUpperCase();
            pushEvent('KeyDown "' + name + '"');
        }
    }

    function keyupEvent(e) {
        e.preventDefault();
        pressedKeys[e.keyCode] = false;
        var name = e.key.toUpperCase();
        pushEvent('KeyUp "' + name + '"');
    }

    // ------- Session storage --------

    var sessionId = null;

    function getStartTimeJSON() {
        //storage = window.sessionStorage;
        storage = window.localStorage;
        return storage.getItem("drawing.clientId");
    }

    function setStartTimeJSON(newSTJ) {
        //storage = window.sessionStorage;
        storage = window.localStorage;
        storage.setItem("drawing.clientId", newSTJ);
        if (debugMode) {
            appCgiW.put("(startTime=" + newSTJ + ", sessionId=" + sessionId + ")");
        }
    }

    // ------- Push events --------

    var appCgiW = mkTextW($("#app-cgi"), "");
    var debugMode = false;

    var eventTimeStart = null;     // absolute real time (ms) at the begin of the activity

    var pendingResponse = false;
    var pendingEvents = [];
    var pendingEventsW = mkTextW($("#pendEvts-output"), 0);
    var lastEventW = mkTextW($("#lastEvt-output"), "");
    var lastEvTimeW = mkTextW($("#time"), 0);

    function pushEvent(ev) {
        var time = Date.now() - eventTimeStart;
        pendingEvents.push({time: time / 1000.0, event: ev});
        pendingEventsW.put(pendingEvents.length);
        lastEventW.put(ev);
        // Force sending now
        sendEvents();
    }

    // Send pending events
    function sendEvents() {
        if (!pendingResponse) { //*** && pendingEvents.length > 0) {
            var time = Date.now() - eventTimeStart;
            lastEvTimeW.put(time / 1000.0);
            var reqdata =
                    { sessionId: sessionId
                    , time: time/1000.0
                    , events: pendingEvents
                    };
            pendingEvents = [];
            pendingEventsW.put(0);
            pendingResponse = true;
            rcvBufferW.put('WAITING');
            $.ajax('./events', {
                type: "POST",
                contentType: 'application/json',
                data: JSON.stringify(reqdata),
                success: responseReceived,
                error: function(jqXHR, textStatus, errorThrown) {
                    alert(textStatus+": "+errorThrown);
                }
            });
        }
    }

    var OLD_resend = false;

    function responseReceived(data) {
        pendingResponse = false;
        rcvBufferW.put('READY');
        if (data.error) {
            alert("responseReceived: error="+data.error);
        } else if (data.draw != null && lastDrawTime <= data.time) {
            if (rcvBuffer == null || rcvBuffer.time <= data.time) {
                // Saves the new draw
                rcvBuffer = data;
                if (OLD_resend) {
                    // Refresh the new draw
                    resfreshSvg();
                    // Send pending events (if any)
                    if (pendingEvents.length > 0) { sendEvents(); }
                } else {
                    // Send pending events (if any)
                    if (pendingEvents.length > 0) { sendEvents(); }
                    // Refresh the new draw
                    resfreshSvg();
                }
            } else {
                // Discards the new draw
                alert("responseReceived: rcvBuffer.time="+rcvBuffer.time+", data.time="+data.time);
            }
        } else {
            alert("responseReceived: lastDrawTime="+lastDrawTime+", data.time="+data.time);
        }
    }

    // ------- Send events & Refresh canvas --------

    var refreshTimer = null;
    var maxRefreshBySecond = 15;
    var refreshBySecondW = mkTextW($("#refr-output"), 10);
    var refreshRangeW = mkRangeW($("#refr-range"), onRefrRangChange);

    function onRefrRangChange(r) {
        if (r < 0) r = 0;
        if (r > 100) r = 100;
        var n = Math.trunc(maxRefreshBySecond * r / 100);
        setRefreshBySecond(n);
    }

    function setRefreshBySecond(n) {
        if (n < 0) n = 0;
        n = Math.trunc(n);
        refreshBySecondW.put(n);
        refreshRangeW.put(n * 100.0 / maxRefreshBySecond);
        // restart refresh timer
        if (refreshTimer) clearInterval(refreshTimer);
        if (n > 0) {
            var interval = 1000.0 / n;
            refreshTimer = setInterval(clockFun, interval);
        }
    }

    function clockFun() {
        // Force send events
        sendEvents();
        resfreshSvg();
    }

    var lastDrawTime = 0;
    var rcvBuffer = null;
    var rcvBufferW = mkTextW($("#rcvBuf-output"), '---');

    function resfreshSvg() {
        // Get SVG from the buffer and show it
        if (rcvBuffer) {
            var data = rcvBuffer;
            rcvBuffer = null;
            rcvBufferW.put('DRAWING');
            lastDrawTime = data.time;
            $("#canvas").html(renderSvgText(data.viewXMax, data.viewYMax, data.draw));
            var svgElem = $("#canvas > svg");
            svgElem.on('keydown', keydownEvent);
            svgElem.on('keyup', keyupEvent);
            svgElem.on('mousedown', function(e){ mousedownEvent(data.viewXMax, data.viewYMax, data.unitSize, e); });
            svgElem.on('mouseup', function(e){ mouseupEvent(data.viewXMax, data.viewYMax, data.unitSize, e); });
            svgElem.on('mousemove', function(e){ mousemoveEvent(data.viewXMax, data.viewYMax, data.unitSize, e); });
            rcvBufferW.put('READY');
        }
    }

    // ------- Stop application --------

    function stopFun() {
        if (sessionId) {
            var reqdata = { sessionId: sessionId };
            $.ajax('./stop', {
                type: "POST",
                contentType: 'application/json',
                data: JSON.stringify(reqdata),
                success: function(data) { }
            });
            sessionId = null;
        }
    }

    // ------- Start application --------

    var urlParams = new URLSearchParams(window.location.search);
    debugMode = urlParams.get('mode') == 'debug';
    window.addEventListener('unload', stopFun);
    $(window).on('keydown', keydownEvent);
    $(window).on('keyup', keyupEvent);
    // Send start action
        pendingResponse = true;
        $.ajax('./start', {
            type: "POST",
            contentType: 'application/json',
            data: JSON.stringify({ startTime: getStartTimeJSON() }),
            success: function(data) {
                eventTimeStart = Date.now();
                if (data.version != "DRAWING/0.2") {
                    alert("      Error de protocol.\nSi us plau, recompileu el CGI");
                    return;
                }
                sessionId = data.sessionId;
                setStartTimeJSON(data.startTime);
                data.time = 0; responseReceived(data);
            },
            error: function(jqXHR, textStatus, errorThrown) {
                    alert(textStatus+": "+errorThrown);
            }
        });
    // Start activity & resfresh
    if (debugMode) {
        setRefreshBySecond(0);
    } else {
        setRefreshBySecond(10);
    }
}

