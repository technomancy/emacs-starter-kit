
// © Copyright 2006 Lennart Borgman, http://www.OurComments.org/. All rights reserved.
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 3, or (at
// your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth
// Floor, Boston, MA 02110-1301, USA.


var HTML_WTOC_NS_sCurrTocId;


HTML_WTOC_NS = {

        /////////////////////////////
        //// Basic event functions
        /////////////////////////////

        getEventObject : function (ev) {
                var o;
                if (window.event)
                        o = window.event.srcElement;
                else if (null != ev)
                        o = ( ev.target );
                return o;
        },
        getEvent : function (ev) {
                if (window.event) {
                        return window.event;
                } else if (null != ev) {
                        return ev;
                }
        },

        eventStopPropagation : function (e) {
          if (e.stopPropagation)
                e.stopPropagation();
          else
                e.cancelBubble=true;
        },

        eventPreventDefault : function (e) {
          if (e.preventDefault)
                e.preventDefault();
          else
                e.returnValue=false;
        },

        /////////////////////////////
        //// TOC hide
        /////////////////////////////

        show_content : function (on) {
                var toc = document.getElementById("html-wtoc-id-toccol").style;
                var tdv = document.getElementById("html-wtoc-id-tocdiv").style;
                var shw = document.getElementById("html-wtoc-id-showtoc").style;
                var hid = document.getElementById("html-wtoc-id-hidetoc").style;
                if (on) {
                        toc.display = "";
                        tdv.display = "";
                        shw.display = "none";
                        hid.display = "";
                        HTML_WTOC_NS.focus_page_link(0);
                } else {
                        toc.display = "none";
                        tdv.display = "none";
                        shw.display = "";
                        hid.display = "none";
                }
        },





        /////////////////////////////
        //// Open-Close
        /////////////////////////////
        onblur_action : function(ev) {
                HTML_WTOC_NS_sCurrTocId = null;
        },
        onfocus_action : function(ev) {
                var o = HTML_WTOC_NS.getEventObject(ev);
                if (!o) return;

                HTML_WTOC_NS_sCurrTocId = o.id;
        },
        onclick_action : function(ev) {
                var o = HTML_WTOC_NS.getEventObject(ev);
                var e = HTML_WTOC_NS.getEvent(ev);
                if (13 == e.keyCode) return true;
                if (!o) return true;
                if ("IMG" == o.tagName) o = o.parentNode;
                var iId = HTML_WTOC_NS.getIdnumFromId(o.id);
                var sChildId = "toc_child_"+iId;
                var sOldCurrTocId = HTML_WTOC_NS_sCurrTocId;
                HTML_WTOC_NS.toggle_open(sChildId, o);
                HTML_WTOC_NS_sCurrTocId = sOldCurrTocId;
                return false;
        },

        toggle_open : function (id, parent) {
                var child = document.getElementById(id).style;
                var sInner = parent.innerHTML;
                var re = new RegExp("[^/]*\.gif", "i");
                if ("none" == child.display) {
                        child.display = "";
                        parent.innerHTML = sInner.replace(re, "down.gif")+"";
                } else {
                        child.display = "none";
                        parent.innerHTML = sInner.replace(re, "right.gif")+"";
                }
        },



        /////////////////////////////
        //// Load
        /////////////////////////////

        onload_actions : function (iPageNum) {
                document.body.onkeydown   = HTML_WTOC_NS.onkeydown_action;
                document.body.onmouseover = HTML_WTOC_NS.onmouseover_action;
                var aATags = document.getElementsByTagName("a");
                for(var i = 0; i < aATags.length; i++) {
                        var o = aATags[i];
                        if (null != HTML_WTOC_NS.getIdnumFromId(o.id)) {
                                o.onfocus = HTML_WTOC_NS.onfocus_action;
                                o.onblur  = HTML_WTOC_NS.onblur_action;
                                if (o.id.substr(0, 12) == "opener_text_") {
                                        o.onclick = HTML_WTOC_NS.onclick_action;
                                        o.title = "Open/Close";
                                } else if (o.id.substr(0, 7) == "opener_") {
                                        o.onclick = HTML_WTOC_NS.onclick_action;
                                        o.className = "html-wtoc-mark";
                                        o.title = "Open/Close";
                                }
                        }
                }
                HTML_WTOC_NS.focus_page_link(iPageNum);
        },
        focus_page_link : function (iPageNum) {
                // Element might be hidden
                try {
                        document.getElementById("toc_link_"+iPageNum).focus();
                } catch (exc) {
                }
        },







        /////////////////////
        //// Mouse
        /////////////////////

        onmouseover_action : function (ev) {
                if (null == HTML_WTOC_NS_sCurrTocId) return true;
                var o = HTML_WTOC_NS.getEventObject(ev);
                var iId = HTML_WTOC_NS.getIdnumFromId(o.id);
                if (null == iId) return true;
                o.focus();
        },



        /////////////////////
        //// Key
        /////////////////////

        onkeydown_action: function (ev) {
                var keyDown    = 40;
                var keyUp      = 38;
                var keyLeft    = 37;
                var keyRight   = 39;
                var keyReturn  = 13;
                var keyF2      = 113;
                var keyInsert  = 45;
                // Opera
                var keyOperaDown    = 57386;
                var keyOperaUp      = 57385;
                var keyOperaLeft    = 57387;
                var keyOperaRight   = 57388;
                var keyOperaF2      = 57346;
                var keyOperaInsert  = 57394;

                var SwitchKey      = keyInsert;
                var SwitchKeyOpera = keyOperaInsert;

                var bUp;
                var e = HTML_WTOC_NS.getEvent(ev);
                if (null == HTML_WTOC_NS_sCurrTocId) {
                        switch (e.keyCode) {
                                case SwitchKey:
                                case SwitchKeyOpera:
                                        HTML_WTOC_NS.focus_page_link(0);
                                        HTML_WTOC_NS.eventStopPropagation(e);
                                        HTML_WTOC_NS.eventPreventDefault(e);
                                        return false;
                        }
                        return true;
                }
                switch (e.keyCode) {
                        case keyLeft:
                        case keyOperaLeft:
                        case keyRight:
                        case keyOperaRight:
                                HTML_WTOC_NS.handle_leftright_keys(e);
                                HTML_WTOC_NS.eventStopPropagation(e);
                                HTML_WTOC_NS.eventPreventDefault(e);
                                return false;
                        case keyDown:
                        case keyOperaDown:
                                bUp = false;
                                break;
                        case keyUp:
                        case keyOperaUp:
                                bUp = true;
                                break;
                        case SwitchKey:
                        case SwitchKeyOpera:
                                if (null != HTML_WTOC_NS_sCurrTocId) {
                                        var o = document.getElementById(HTML_WTOC_NS_sCurrTocId);
                                        if (o) o.blur();
                                        HTML_WTOC_NS_sCurrTocId = null;
                                }
                                HTML_WTOC_NS.eventStopPropagation(e);
                                HTML_WTOC_NS.eventPreventDefault(e);
                                return false;
                        default:
                        //alert(e.keyCode);
                                return true;
                }
                var oOpener;
                oOpener = HTML_WTOC_NS.getNextVisOpener(HTML_WTOC_NS_sCurrTocId, bUp);
                oOpener.focus();
                HTML_WTOC_NS.eventStopPropagation(e);
                HTML_WTOC_NS.eventPreventDefault(e);
                return false;
        },

        handle_leftright_keys: function (e) {
                var keyLeft  = 37;
                var keyRight = 39;
                var keyOperaLeft  = 57387;
                var keyOperaRight = 57388;
                var iId = HTML_WTOC_NS.getIdnumFromId(HTML_WTOC_NS_sCurrTocId);
                if (null == iId) return;
                var sId = "opener_" + iId;
                var oOpener = document.getElementById(sId);
                var sId = HTML_WTOC_NS_sCurrTocId;      // It will be cleared before getNextVis

                var bOpenAction;
                var bOpened;
                var bUp;
                var oChild = document.getElementById("toc_child_"+iId);
                if (null == oChild) {
                } else {
                        bOpened = (oChild.style.display != "none");
                }
                switch (e.keyCode) {
                        case keyLeft:
                        case keyOperaLeft:
                                bUp = true;
                                bOpenAction = (null != bOpened) && (bOpened);
                                break;
                        case keyRight:
                        case keyOperaRight:
                                bUp = false;
                                bOpenAction = (null != bOpened) && (!bOpened);
                                break;
                        default:
                                alert("bad key handling...");
                }
                if (bOpenAction) {
                        oOpener.click();
                        HTML_WTOC_NS_sCurrTocId = sId;
                } else {
                        var oPrev = HTML_WTOC_NS.getNextVisOpener(sId, bUp);
                        oPrev.focus();
                }
        },






        //////////////////////
        //// Util
        //////////////////////
        getNameFromId: function (sId) {
                var re = new RegExp("(.*?_)(\\d+)", "i");
                if (!re.test(sId)) return null;
                var iId = sId.replace(re, "$1");
                return iId;
        },
        getIdnumFromId: function (sId) {
                var re = new RegExp("(.*?_)(\\d+)", "i");
                if (!re.test(sId)) return null;
                var iId = sId.replace(re, "$2");
                return iId;
        },


        getNextVisOpener: function (sId, bUp, bTrace) {
                if (bTrace) alert("getNextVisOpener("+sId+","+bUp+")");
                var iId = HTML_WTOC_NS.getIdnumFromId(sId);
                if (null == iId) {
                        alert("getNextVisOpener err iId==null");
                        return;
                }
                var sIdName = HTML_WTOC_NS.getNameFromId(sId);
                if (null == sIdName) {
                        alert("getNextVisOpener err sIdName==null");
                        return;
                }
                var oOpener;
                var iLoop = -2;
                while (oOpener == null) {
                        if (bTrace) alert(iId);
                        if (iLoop++ > iMaxChildNum) { alert("Child num error"); return; }
                        if (!bUp) {
                                iId++;
                        } else {
                                iId--;
                        }
                        if (iId > iMaxChildNum) { iId = 0; }
                        if (iId <  0) { iId = iMaxChildNum; }
                        var s = sIdName+iId;
                        oOpener = document.getElementById(s);
                        if (oOpener != null) {
                                if (bTrace) alert(oOpener.offsetLeft);
                                if (oOpener.style.display == "none") { // All
                                        oOpener = null;
                                } else if (oOpener.offsetLeft < 0) { // IE
                                        oOpener = null;
                                } else if (0 == oOpener.scrollWidth) { // Opera
                                        oOpener = null;
                                }
                        }
                }
                return oOpener;
        }



}; //HTML_WTOC_NS
