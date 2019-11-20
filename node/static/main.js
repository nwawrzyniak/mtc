'use strict';
(function() {
  var container,
      msgTpl = $('<div class="msg"></div>'),
      loadMsg = function() {
        container.empty().text('Loading messages');
        $.ajax({
          url: "/api/get",
          method: "get",
          success: function(data) {
            if(Array.isArray(data)) {
              container.empty();
              data.forEach(msg => container.append(msgTpl.clone().html(msg.msg.replace(/(\r\n|\n\r|\r|\n)/g, '<br>$1'))));
            }
          }
        });
      };
  $(document).ready(function() {
    container =  $('#msgs');
    loadMsg();
    var form = $('form'),
        textarea = $('#msgfield').on('keypress', function(e) {
          if(!e.shiftKey && e.which == '13') {
            e.preventDefault();
            form.submit();
          }
        });
    form.on('submit', function(e) {
      e.preventDefault();
      $.ajax({
        url: "/api/msg",
        method: "post",
        data: form.serialize(),
        success: function(data) {
          textarea.prop('disabled', false);
          if(typeof data.success !== 'undefined' && data.success == 'true') {
            textarea.val('');
            location.reload();
          }
        }
      });
      textarea.prop('disabled', true);
    });
  });
})();
