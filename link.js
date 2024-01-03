window.onload = function() {
    document.querySelectorAll('a').forEach(function(link) {
        link.setAttribute('target', '_blank');
    });
};

window.onload = function() {
    document.querySelectorAll('.quarto-header a').forEach(function(link) {
        link.setAttribute('target', '_self');
    });
};
