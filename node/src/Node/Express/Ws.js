// module Node.Express.Ws
'use strict';

exports._listenHostHttpWs = function(appInit) {
    return function(port) {
        return function(host) {
            return function(cb) {
                return function() {
                    var app = require('express')();
                    var expressWs = require('express-ws')(app, null,
                                                    {leaveRouterUntouched: true});
                    var server = expressWs.getWss();
                    appInit(app)();
                    app.listen(port, host, function(e) {
                        return cb(e)();
                    });
                    return server;
                }
            }
        }
    }
};

exports._ws = function (app, route, handler) {
  return function () {
    return app.ws(route, function(ws, req) {
      return handler(ws)(req)();
    });
  }
};

exports._onMessage = function(ws) {
  return function(cb) {
    return function() {
      return ws.on('message', function(msg) {
        return cb(ws)(msg)();
      });
    }
  }
};

exports._onClose = function(ws) {
  return function(cb) {
    return function() {
      return ws.on('close', function() {
        return cb();
      });
    }
  }
};

exports._send = function(ws) {
  return function(msg) {
    return function() {
      return ws.send(msg);
    }
  }
}
