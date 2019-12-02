// module Node.Express.Ws
'use strict';

exports.mkApplication = function() {
    var express = require('express');
    return express();
}

/*exports._httpServer = function(app) {
    return function() {
        var http = require('http');
        var server = http.createServer(app);
        return server;
    }
}*/

exports._listenHttpWs = function(app) {
    return function(port) {
        return function(cb) {
            return function() {
                var http = require('http');
                var server = http.createServer(app);
                var expressWs = require('express-ws')(app, server,
                                                {leaveRouterUntouched: true});
                server.listen(port, function(e) {
                    return cb(e)();
                });
                return server;
            }
        }
    }
}

exports._ws = function (app, route, handler) {
  return function () {
    return app.ws(route, function(ws, req) {
      return handler(ws)(req)();
    });
  };
};

exports._onMessage = function(ws) {
  return function(cb) {
    return function() {
      return ws.on('message', function(msg) {
        return cb(ws)(msg)();
      });
    }
  }
}

exports._send = function(ws) {
  return function(msg) {
    return function() {
      return ws.send(msg);
    }
  }
}
