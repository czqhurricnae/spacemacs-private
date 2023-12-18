var subtitle_footer = document.createElement('footer');
subtitle_footer.setAttribute('id', 'render-footer');
// subtitle_footer.setAttribute('style', 'background: rgba(0,0,127,0.1)')
var subtitle_footer_main = document.createElement('div');
subtitle_footer_main.setAttribute('id', 'render-div');
subtitle_footer.appendChild(subtitle_footer_main);

var div_class_reveal=document.querySelectorAll('.reveal')[0];
div_class_reveal.appendChild(subtitle_footer);
