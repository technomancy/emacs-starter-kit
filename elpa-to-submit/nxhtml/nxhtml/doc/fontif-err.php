<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN"
"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>nXhtml Notes and Changes</title>
    <link href="wd/grapes/nxhtml-grapes.css" rel="StyleSheet" type="text/css" />
  </head>
  <body>
    <div id="container">

      <div id="rgtcol">
        <p id="nxhtml-home">
          <a href="nxhtml.html">To nXhtml main page</a>
        </p>

        <h1>nXhtml Notes and Bugs</h1>

        <dl>
<!--           <dt id="bugs-affect-mode-switching">Two Emacs 22 beta bugs affects PHP mode switching</dt> -->
<!--           <dd> -->
<!--             <p> -->
<!--             <a href="nxhtml.html#php">PHP / nXhtml automatic mode switching</a> is affected: -->
<!--             </p> -->
<!--             <ul> -->
<!--               <li> -->
<!--                 Because of a bug in Emacs 22 beta you may have to turn -->
<!--                 off and on the switch <em>Mode Switching at &lt;? -->
<!--                 ... ?></em> in the menus to get the automatic mode -->
<!--                 switching to start. -->
<!--                 <em>(Work around added in 0.94)</em> -->
<!--               </li> -->
<!--               <li> -->
<!--                 There is the same problem with showing XML path. -->
<!--                 <em>(Work around added in 0.94)</em> -->
<!--               </li> -->
<!--               <li> -->
<!--                 It also affects MLinks. -->
<!--                 <em>(Work around added in 0.94)</em> -->
<!--               </li> -->
<!--               <li> -->
<!--                 Because of another bug in Emacs 22 beta immediately -->
<!--                 after mode the automatic mode switching the keyboard -->
<!--                 uses the key bindings from the <em>wrong mode for the -->
<!--                 first key</em>. Type any key on the keyboard, that -->
<!--                 cures it. -->
<!--                 <em>(Work around added in 0.95)</em> -->
<!--               </li> -->
<!--             </ul> -->
<!--           </dd> -->
          <dt id="new-mode-switching" style="margin-top:1em;">I have rewritten the PHP mode switching</dt>
          <dd>
            <p>
              Because of some (fair) critique I have gotten about the way mode switching between php-mode and nxhtml-mode works
              I have rewritten that part.
              The new mode switching also includes embedded css, javascript, eRuby and JSP.
            </p>
          </dd>
          <dt id="hadron-bugs" style="margin-top:1em;">A lot of bugs corrected for version 0.98</dt>
          <dd>
            <p>
              I want to thanks Hadron Quark for helping me by testing
              and pointing out bugs and weaknesses, most related to
              editing of PHP.  I have included fixes for many of them
              in version 0.98 and more may follow.
            </p>
          </dd>
        </dl>

        <h1>nXhtml Changes</h1>

        <dl>
          <dt>0.89</dt>
          <dd>
            <ul>
              <li>
                Corrected autostart for nXhtml when not used together with EmacsW32.
              </li>
            </ul>
          </dd>
          <dt>0.90</dt>
          <dd>
            <ul>
              <li>
                Improved display of XML path.
              </li>
              <li>
                Discontinued xmple-mode.
              </li>
              <li>
                New major modes nxhtml-part-mode/nxml-part-mode replaces
                minor mode xmlpe-mode.  (While moving the code to
                nxhtml-part.el I also fixed a bug in Xmple minor mode that
                made Emacs take 99% of the CPU.)
              </li>
            </ul>
          </dd>
          <dt>0.91</dt>
          <dd>
            <ul>
              <li>
                Fixed some calls to perl which prevented uploading of
                a site of you did not have perl in the same location
                as me.
              </li>
              <li>
                Glued together things so that editing PHP files works
                as I intended. (This means that Emacs switches between
                php-mode and nxhtml-part-mode automatically when
                moving point. And that you can use completion.)
              </li>
              <li>
                Starting working on the documentation for nXhtml.
                New layout to the documentation files.
                Examples with images.
              </li>
            </ul>
          </dd>
          <dt>0.92</dt>
          <dd>
            <ul>
              <li>
                Fixes to make the switching between php and xhtml
                style editing work better.
              </li>
            </ul>
          </dd>
          <dt>0.93</dt>
          <dd>
            <ul>
              <li>
                Better error handling when switching to editing
                embedded JavaScript and CSS.
              </li>
              <li>
                Removed PHP spec from embedded switching since they
                interfered with the automatic switching between php
                and xhtml.
              </li>
              <li>
                Gives an error message if web host is not defined in
                site when trying to use View Uploaded File and
                cousins.
              </li>
              <li>
                Gives a ready message when finished uploading a single
                file.
              </li>
              <li>
                When using Mode Switching at &lt;? ... ?> mode
                switching could occur in wrong buffer. Fixed together
                with some other buffer problems.
              </li>
            </ul>
          </dd>
          <dt>0.94</dt>
          <dd>
            <ul>
              <li>
                Add http://www.w3.org/ to the help sites for CSS.
              </li>
              <li>
                Included a CSS mode.
              </li>
              <li>
                Added a menu entry for bug reporting.
              </li>
              <li>
                Renamed menu bar entry from XHTML to nXhtml for clarity.
                (But nXml menu bar entry is still called XML.)
              </li>
              <li>
                Added work around for globalized minor modes in the
                cases of MLinks, XML Path and mode switching at &lt;? ... ?>.
              </li>
            </ul>
          </dd>
          <dt>0.95</dt>
          <dd>
            <ul>
              <li>
                Added workaround for the problem with the first
                keyboard key after automatically switching of mode at
                &lt;? ... ?>.
              </li>
            </ul>
          </dd>
          <dt>0.96</dt>
          <dd>
            <ul>
              <li>
                Added support for multiple major modes with mumamo.el.
              </li>
              <li>
                More conventient handling of links. They can now be
                opened in the same window, 'other window' or in a new
                frame.
              </li>
            </ul>
          </dd>
          <dt>0.97</dt>
          <dd>
            <ul>
              <li>
                Schema was not setup after starting new page so
                completion did not work. Fixed.
              </li>
              <li>
                Added http://xhtml.com/ to help sites for XHTML.
              </li>
              <li>
                Added the concept of <em>XML validation headers</em>.
                These are just text parsed by the nXml validation
                parser to get a start state before starting parsing a
                buffer.  This allows the use of the nXml completion in
                buffers where there are no XML header.  Such a header
                is often lacking for example in PHP code since the
                XHTML header is often generated dynamically.
              </li>
              <li>
                Because of the change above <em>nxhtml-part-mode</em>
                is no longer needed and is therefore declared
                obsolete.
              </li>
              <li>
                Corrected a bug in mlinks.el that prevented opening an
                HTML link in a other window or a new frame.
              </li>
              <li>
                Added support for JSP, eRuby and some support for perl
                in mumamo.el.
              </li>
            </ul>
          </dd>
          <dt>0.98</dt>
          <dd id="0.98">
            <ul>
              <li>
                Mumamo was not found when nXhtml was installed with
                just the zip file. Corrected.  (nXhtml is also
                installed when you install EmacsW32.)
              </li>
              <li>
                Enhancement to mumamo error handling when a bad mode
                specifier for an embedded mode is found.
              </li>
              <li>
                Introduced a bug for empty XHTML documents in
                0.97. Corrected.
              </li>
              <li>
                Corrected a bug for chunks 1 character long.
              </li>
              <li>
                There is a bug in Emacs 22 in the handling of global
                minor mode that are not distributed with Emacs. If
                they are turned on by customization, but loaded after
                Emacs have loaded the customizations (usually in
                .emacs) then they are not turned on correctly. Added
                work-around for this.
              </li>
              <li>
                <em>Extra XHTML Validation Header</em>:
                <ul>
                  <li>
                    <em>Extra XHTML Validation Header</em> state was not saved when moving between chunks. Fixed.
                  </li>
                  <li>
                    Tried to make the concept of <em>Extra XHTML Validation Header</em>
                    more clear.  Added this visually to the buffer.
                  </li>
                  <li>
                    <em>Extra XHTML Validation Headers</em> can now be turned on
                    automatically based on file name.
                  </li>
                </ul>
              </li>
              <li>
                <em>nXhtml menu:</em>
                <ul>
                  <li>
                    Reorganized the nXhtml menu.
                  </li>
                  <li>
                    Added <em>customization</em> groups for help libraries to nXhtml.
                  </li>
                  <li>
                    Added an entry for customization of nXhtml to the menus.
                  </li>
                  <li>
                    Added <em>Tidy</em> to the menus again.
                  </li>
                </ul>
              </li>
              <li>
                Corrected bug in <em>XML Path</em> (nxml-where) for single tags.
                Other small fixes to nxhtml-where.
              </li>
              <li>
                Documentation enhancements.
                Added <em>The Quick Guide</em>.
              </li>
              <li>
              </li>
            </ul>
          </dd>
          <dt>0.99</dt>
          <dd id="0.99">
            <ul>
              <li>
                Corrections to indentation.
              </li>
              <li>
                Turn on mumamo-mode by file name.
              </li>
              <li>
                The Extra XHTML Validation Header state were not saved when changing major mode in MuMaMo. Corrected.
              </li>
              <li>
                Added more alternatives to the Extra XHTML Validation Header list.
                This should make it easier to use completion with for example PHP.
              </li>
              <li>
                Added default value for the Extra XHTML Validation Header.
              </li>
            </ul>
          </dd>
        </dl>
      </div>
    </div>

    <hr class="footer"/>
    <p class="footer">
      Copyright &copy; 2007 OurComments.org
      --
      Latest update 2007-04-11
    </p>
  </body>
</html>
