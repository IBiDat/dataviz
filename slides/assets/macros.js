remark.macros.vspace = function(px) {
  return '<div style="height: ' + px + 'px"></div>';
}

remark.macros.hspace = function(px) {
  return '<span style="margin-right: ' + px + 'px"></span>';
}

remark.macros.scale = function(percentage) {
  return '<img src="' + this + '" style="width: ' + percentage + '" />';
};

remark.macros.bqh = function() {
  return '<blockquote class="bqh">' + this + '</blockquote>';
};

remark.macros.tail = function(x) {
  return '<div class="research-left-column tail ' + x + '"><h2>' +
    this + '</h2></div>';
};

remark.macros.arrow = function(dir, x, y) {
  return '<div class="arrow arrow-' + dir + '" style="top:' + y + ';left:' + x +
  '"><strong>' + this + '</strong></div>'
};
