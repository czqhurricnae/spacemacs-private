var merriamWebsterWrap = function (toWrap, wrapper) {
    wrapper = wrapper || document.createElement('a');
    toWrap.parentNode.appendChild(wrapper);
    return wrapper.appendChild(toWrap);
};
Array.from(document.getElementsByClassName('thread-anchor-content')).forEach(e => { merriamWebsterWrap(e, '') });
Array.from(document.getElementsByClassName('has-aq')).forEach(e => { merriamWebsterWrap(e, '') });
