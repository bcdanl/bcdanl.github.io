window.onload = function() {
    document.addEventListener('DOMContentLoaded', function() {
    var navbarLinks = document.querySelectorAll('.navbar-container a'); // Replace '#navbar' with your navbar's ID or class
    navbarLinks.forEach(function(link) {
        link.removeAttribute('target'); // Remove any existing target attributes
        link.setAttribute('target', '_self'); // Set target to '_self'
    });
});
};