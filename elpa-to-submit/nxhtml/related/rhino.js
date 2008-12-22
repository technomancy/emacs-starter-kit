// Where you store your files
var project_folder = 'c:/emacs/p/070604/EmacsW32/nxml/related/';
//var project_folder = '/';
// Browser environment wrapper over Rhino
load(project_folder + 'env.js');
// For DOM constructing
window.location = project_folder + 'blank.html';
var my_script = arguments[0];
// If DOM ready
window.onload = function(){
    // Avoid recursive inclusion
    if ("rhino_flymake.js" != my_script) {
        load(my_script);
    }
}
