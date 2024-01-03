document.addEventListener('keydown', function(event) {
    if (event.key === 'b') { // 'b' key for back
        window.history.back();
    }
});

Reveal.on( 'fragmentshown', event => {
    event.fragment.classList.remove("fragment");
} );