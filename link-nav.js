document.addEventListener('DOMContentLoaded', function() {
    document.querySelectorAll('#nav-fixed a').forEach(function(link) {
        link.setAttribute('target', '_self');
    });
});
