// automagic toc: use .intoc to add h1 to toc, .toc to display
document.addEventListener('DOMContentLoaded', function(event) {
  var toc = document.getElementsByClassName('toc')[0];
  if (toc === undefined) return;

  var slides = document.getElementsByClassName('remark-slide-content');
  var ul = document.createElement('ul');

  slides.forEach(function(x, i) {
    if (!x.classList.contains('intoc')) return;
    if (i + 1 === slides.length) return;

    let a = document.createElement('a');
    a.href = "#" + (i + 1);
    a.textContent = x.getElementsByTagName('h1')[0].textContent;
    let li = document.createElement('li');
    li.appendChild(a);
    ul.appendChild(li);
  });

  toc.appendChild(ul);
});

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
