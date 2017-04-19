/*
 * Copyright 2012 The Board of Regents of the University of Wisconsin System.
 * Contributors: Jason Shao, James McCurdy, Enhai Xie, Adam G.W. Halstead,
 * Michael H. Whitney, Nathan DiPiazza, Trey K. Sato and Yury V. Bukhman
 *
 * This file is part of GCAT.
 *
 * GCAT is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GCAT is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with GCAT.  If not, see <http://www.gnu.org/licenses/>.
 */

var jQueryLoaderOptions=null;
(function(a){a.loader=function(d){switch(d){case"close":if(jQueryLoaderOptions){if(a("#"+jQueryLoaderOptions.id)){a("#"+jQueryLoaderOptions.id+", #"+jQueryLoaderOptions.background.id).remove()
}}return;
break;
case"setContent":if(jQueryLoaderOptions){if(a("#"+jQueryLoaderOptions.id)){if(a.loader.arguments.length==2){a("#"+jQueryLoaderOptions.id).html(a.loader.arguments[1])
}else{if(console){console.error("setContent method must have 2 arguments $.loader('setContent', 'new content');")
}else{alert("setContent method must have 2 arguments $.loader('setContent', 'new content');")
}}}}return;
break;
default:var b=a.extend({content:"<div>Loading ...</div>",className:"loader",id:"jquery-loader",height:100,width:200,zIndex:30000,background:{opacity:0.4,id:"jquery-loader-background"}},d)
}jQueryLoaderOptions=b;
var c=a(document).height();
var e=a(window).width();
var g=a('<div id="'+b.background.id+'"/>');
g.css({zIndex:b.zIndex,position:"absolute",top:"0px",left:"0px",width:e,height:c,opacity:b.background.opacity});
g.appendTo("body");
if(jQuery.bgiframe){g.bgiframe()
}var f=a('<div id="'+b.id+'" class="'+b.className+'"></div>');
f.css({zIndex:b.zIndex+1,width:b.width,height:b.height});
f.appendTo("body");
f.center();
a(b.content).appendTo(f)
};
a.fn.center=function(){this.css("position","absolute");
this.css("top",(a(window).height()-this.outerHeight())/2+a(window).scrollTop()+"px");
this.css("left",(a(window).width()-this.outerWidth())/2+a(window).scrollLeft()+"px");
return this
}
})(jQuery);