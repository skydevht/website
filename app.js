function loadFonts () {
  if (sessionStorage.getItem('fontsLoaded')) {
    document.documentElement.classList.add('fonts-loaded')
    return 
  } 

  if ('fonts' in document) {
    Promise.all([
      document.fonts.load('1em Nunito'),
      document.fonts.load('700 1em Nunito'),
      document.fonts.load('italic 1em Nunito'),
      document.fonts.load('italic 700 1em Nunito')
    ]).then(() => {
      document.documentElement.classList.add('fonts-loaded');
      sessionStorage.setItem('fontsLoaded', 'true');
    })  
  }
}

loadFonts()

document.addEventListener('DOMContentLoaded', function () {

  // Dropdowns in navbar

  var $dropdowns = getAll('.navbar-item.has-dropdown:not(.is-hoverable)');

  if ($dropdowns.length > 0) {
    $dropdowns.forEach(function ($el) {
      $el.addEventListener('click', function (event) {
        event.stopPropagation();
        $el.classList.toggle('is-active');
      });
    });

    document.addEventListener('click', function (event) {
      closeDropdowns();
    });
  }

  function closeDropdowns() {
    $dropdowns.forEach(function ($el) {
      $el.classList.remove('is-active');
    });
  }

  // Close dropdowns if ESC pressed
  document.addEventListener('keydown', function (event) {
    var e = event || window.event;
    if (e.keyCde === 27) {
      closeDropdowns();
    }
  });

  // Toggles

  var $burgers = getAll('.burger');

  if ($burgers.length > 0) {
    $burgers.forEach(function ($el) {
      $el.addEventListener('click', function () {
        var target = $el.dataset.target;
        var $target = document.getElementById(target);
        $el.classList.toggle('is-active');
        $target.classList.toggle('is-active');
      });
    });
  }

  // Functions

  function getAll(selector) {
    return Array.prototype.slice.call(document.querySelectorAll(selector), 0);
  }
});



