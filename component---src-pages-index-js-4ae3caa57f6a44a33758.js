(window.webpackJsonp=window.webpackJsonp||[]).push([[5],{"2W6z":function(e,t,n){"use strict";var r=function(){};e.exports=r},"HaE+":function(e,t,n){"use strict";function r(e,t,n,r,a,o,i){try{var u=e[o](i),c=u.value}catch(l){return void n(l)}u.done?t(c):Promise.resolve(c).then(r,a)}function a(e){return function(){var t=this,n=arguments;return new Promise((function(a,o){var i=e.apply(t,n);function u(e){r(i,a,o,u,c,"next",e)}function c(e){r(i,a,o,u,c,"throw",e)}u(void 0)}))}}n.d(t,"a",(function(){return a}))},N8Pf:function(e,t,n){"use strict";(function(e){var r=n("HaE+"),a=n("dI71"),o=n("o0o1"),i=n.n(o),u=n("q1tI"),c=n.n(u),l=n("3Z9Z"),s=n("JI6e"),d=n("cWnB"),f=n("wsfE"),v=function(t){function n(e){var n;return(n=t.call(this,e)||this).state={seed:0,pathseed:0,wave:0,scale:0},n}Object(a.a)(n,t);var o=n.prototype;return o.componentDidMount=function(){var t=this;e.CsoundObj.initialize().then((function(){t.csound=new e.CsoundObj,fetch("/vcohousegen_all.csd").then((function(e){e.arrayBuffer().then(function(){var e=Object(r.a)(i.a.mark((function e(n){return i.a.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:t.csound.writeToFS("vcohousegen_all.csd",n),t.csound.compileCSD("vcohousegen_all.csd"),t.csound.start(),t.csoundLoaded=!0,t.started=!0,t.isPlaying=!1;case 6:case"end":return e.stop()}}),e)})));return function(t){return e.apply(this,arguments)}}())}))}))},o.handleStart=function(){if(!this.isPlaying){this.csound.readScore('i "trigDrums" 0 3000'),this.csound.readScore('i "hvs" 0.00067 3000 110 4 0 0 -1 -1'),this.isPlaying=!0;var e=this;setTimeout((function(){e.csound.requestControlChannel("seed",(function(){return e.setState({seed:e.csound.getControlChannel("seed")})})),e.csound.requestControlChannel("pathseed",(function(){return e.setState({pathseed:e.csound.getControlChannel("pathseed")})})),e.csound.requestControlChannel("wave",(function(){return e.setState({wave:Math.trunc(e.csound.getControlChannel("wave"))})})),e.csound.requestControlChannel("scale",(function(){return e.setState({scale:Math.trunc(e.csound.getControlChannel("scale"))})}))}),500)}},o.handlePause=function(){this.started&&(this.started=!1,e.CsoundObj.CSOUND_AUDIO_CONTEXT.suspend())},o.handleContinue=function(){this.started||(this.started=!0,e.CsoundObj.CSOUND_AUDIO_CONTEXT.resume())},o.handleStop=function(){this.isPlaying&&(this.csound.readScore('i "trigOff" 0 0.1'),this.isPlaying=!1)},o.handleUpdate=function(e,t){this.csound.setControlChannel(e,t)},o.render=function(){var e={id:"hvsamp",type:"fader",label:"amp",min:0,max:1,step:.01,defval:1},t={id:"hvsrev",type:"fader",label:"reverb",min:0,max:1,step:.01,defval:.6},n={id:"hvsdel",type:"fader",label:"delay",min:0,max:1,step:.01,defval:.1},r={id:"drums",type:"fader",label:"drums",min:0,max:1,step:.01,defval:.5},a=this.state,o=a.seed,i=a.pathseed,u=a.wave,v=a.scale;return c.a.createElement(c.a.Fragment,null,c.a.createElement(l.a,null,c.a.createElement(s.a,null,c.a.createElement(d.a,{onClick:this.handleStart.bind(this)},"Start")),c.a.createElement(s.a,null,c.a.createElement(d.a,{onClick:this.handlePause.bind(this)},"Pause")),c.a.createElement(s.a,null,c.a.createElement(d.a,{onClick:this.handleContinue.bind(this)},"Continue")),c.a.createElement(s.a,null,c.a.createElement(d.a,{onClick:this.handleStop.bind(this)},"Stop"))),c.a.createElement(l.a,null,c.a.createElement(s.a,null,"seed:",o),c.a.createElement(s.a,null,"pseed:",i),c.a.createElement(s.a,null,"wave:",u),c.a.createElement(s.a,null,"scale:",v)),c.a.createElement(l.a,null,c.a.createElement(s.a,null,c.a.createElement(f.a,{def:e,key:e.id,onChange:this.handleUpdate.bind(this)})),c.a.createElement(s.a,null,c.a.createElement(f.a,{def:t,key:t.id,onChange:this.handleUpdate.bind(this)})),c.a.createElement(s.a,null,c.a.createElement(f.a,{def:n,key:n.id,onChange:this.handleUpdate.bind(this)})),c.a.createElement(s.a,null,c.a.createElement(f.a,{def:r,key:r.id,onChange:this.handleUpdate.bind(this)}))))},n}(c.a.Component);t.a=v}).call(this,n("yLpj"))},RXBc:function(e,t,n){"use strict";n.r(t);var r=n("q1tI"),a=n.n(r),o=(n("Wbzz"),n("Bl7J")),i=n("N8Pf"),u=n("JI6e"),c=n("3Z9Z");t.default=function(){return a.a.createElement(o.a,null,a.a.createElement(u.a,null,a.a.createElement(c.a,null,a.a.createElement(u.a,null," Every time the track is stopped and started again, new random seeds are used to generate")),a.a.createElement(c.a,null,a.a.createElement(u.a,null," ",a.a.createElement(i.a,null))),a.a.createElement(c.a,null,a.a.createElement(u.a,null," (C) 2021 by docB"))))}},cWnB:function(e,t,n){"use strict";var r=n("wx14"),a=n("zLVn"),o=n("TSYQ"),i=n.n(o),u=n("q1tI"),c=n.n(u),l=n("vUet");var s=function(){for(var e=arguments.length,t=new Array(e),n=0;n<e;n++)t[n]=arguments[n];return t.filter((function(e){return null!=e})).reduce((function(e,t){if("function"!=typeof t)throw new Error("Invalid Argument Type, must only provide functions, undefined, or null.");return null===e?t:function(){for(var n=arguments.length,r=new Array(n),a=0;a<n;a++)r[a]=arguments[a];e.apply(this,r),t.apply(this,r)}}),null)};function d(e){return!e||"#"===e.trim()}var f=c.a.forwardRef((function(e,t){var n=e.as,o=void 0===n?"a":n,i=e.disabled,u=e.onKeyDown,l=Object(a.a)(e,["as","disabled","onKeyDown"]),f=function(e){var t=l.href,n=l.onClick;(i||d(t))&&e.preventDefault(),i?e.stopPropagation():n&&n(e)};return d(l.href)&&(l.role=l.role||"button",l.href=l.href||"#"),i&&(l.tabIndex=-1,l["aria-disabled"]=!0),c.a.createElement(o,Object(r.a)({ref:t},l,{onClick:f,onKeyDown:s((function(e){" "===e.key&&(e.preventDefault(),f(e))}),u)}))}));f.displayName="SafeAnchor";var v=f,h=c.a.forwardRef((function(e,t){var n=e.bsPrefix,o=e.variant,u=e.size,s=e.active,d=e.className,f=e.block,h=e.type,p=e.as,m=Object(a.a)(e,["bsPrefix","variant","size","active","className","block","type","as"]),y=Object(l.a)(n,"btn"),g=i()(d,y,s&&"active",o&&y+"-"+o,f&&y+"-block",u&&y+"-"+u);if(m.href)return c.a.createElement(v,Object(r.a)({},m,{as:p,ref:t,className:i()(g,m.disabled&&"disabled")}));t&&(m.ref=t),h?m.type=h:p||(m.type="button");var b=p||"button";return c.a.createElement(b,Object(r.a)({},m,{className:g}))}));h.displayName="Button",h.defaultProps={variant:"primary",active:!1,disabled:!1};t.a=h},ls82:function(e,t,n){var r=function(e){"use strict";var t=Object.prototype,n=t.hasOwnProperty,r="function"==typeof Symbol?Symbol:{},a=r.iterator||"@@iterator",o=r.asyncIterator||"@@asyncIterator",i=r.toStringTag||"@@toStringTag";function u(e,t,n){return Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}),e[t]}try{u({},"")}catch(O){u=function(e,t,n){return e[t]=n}}function c(e,t,n,r){var a=t&&t.prototype instanceof d?t:d,o=Object.create(a.prototype),i=new S(r||[]);return o._invoke=function(e,t,n){var r="suspendedStart";return function(a,o){if("executing"===r)throw new Error("Generator is already running");if("completed"===r){if("throw"===a)throw o;return C()}for(n.method=a,n.arg=o;;){var i=n.delegate;if(i){var u=E(i,n);if(u){if(u===s)continue;return u}}if("next"===n.method)n.sent=n._sent=n.arg;else if("throw"===n.method){if("suspendedStart"===r)throw r="completed",n.arg;n.dispatchException(n.arg)}else"return"===n.method&&n.abrupt("return",n.arg);r="executing";var c=l(e,t,n);if("normal"===c.type){if(r=n.done?"completed":"suspendedYield",c.arg===s)continue;return{value:c.arg,done:n.done}}"throw"===c.type&&(r="completed",n.method="throw",n.arg=c.arg)}}}(e,n,i),o}function l(e,t,n){try{return{type:"normal",arg:e.call(t,n)}}catch(O){return{type:"throw",arg:O}}}e.wrap=c;var s={};function d(){}function f(){}function v(){}var h={};h[a]=function(){return this};var p=Object.getPrototypeOf,m=p&&p(p(D([])));m&&m!==t&&n.call(m,a)&&(h=m);var y=v.prototype=d.prototype=Object.create(h);function g(e){["next","throw","return"].forEach((function(t){u(e,t,(function(e){return this._invoke(t,e)}))}))}function b(e,t){var r;this._invoke=function(a,o){function i(){return new t((function(r,i){!function r(a,o,i,u){var c=l(e[a],e,o);if("throw"!==c.type){var s=c.arg,d=s.value;return d&&"object"==typeof d&&n.call(d,"__await")?t.resolve(d.__await).then((function(e){r("next",e,i,u)}),(function(e){r("throw",e,i,u)})):t.resolve(d).then((function(e){s.value=e,i(s)}),(function(e){return r("throw",e,i,u)}))}u(c.arg)}(a,o,r,i)}))}return r=r?r.then(i,i):i()}}function E(e,t){var n=e.iterator[t.method];if(void 0===n){if(t.delegate=null,"throw"===t.method){if(e.iterator.return&&(t.method="return",t.arg=void 0,E(e,t),"throw"===t.method))return s;t.method="throw",t.arg=new TypeError("The iterator does not provide a 'throw' method")}return s}var r=l(n,e.iterator,t.arg);if("throw"===r.type)return t.method="throw",t.arg=r.arg,t.delegate=null,s;var a=r.arg;return a?a.done?(t[e.resultName]=a.value,t.next=e.nextLoc,"return"!==t.method&&(t.method="next",t.arg=void 0),t.delegate=null,s):a:(t.method="throw",t.arg=new TypeError("iterator result is not an object"),t.delegate=null,s)}function w(e){var t={tryLoc:e[0]};1 in e&&(t.catchLoc=e[1]),2 in e&&(t.finallyLoc=e[2],t.afterLoc=e[3]),this.tryEntries.push(t)}function k(e){var t=e.completion||{};t.type="normal",delete t.arg,e.completion=t}function S(e){this.tryEntries=[{tryLoc:"root"}],e.forEach(w,this),this.reset(!0)}function D(e){if(e){var t=e[a];if(t)return t.call(e);if("function"==typeof e.next)return e;if(!isNaN(e.length)){var r=-1,o=function t(){for(;++r<e.length;)if(n.call(e,r))return t.value=e[r],t.done=!1,t;return t.value=void 0,t.done=!0,t};return o.next=o}}return{next:C}}function C(){return{value:void 0,done:!0}}return f.prototype=y.constructor=v,v.constructor=f,f.displayName=u(v,i,"GeneratorFunction"),e.isGeneratorFunction=function(e){var t="function"==typeof e&&e.constructor;return!!t&&(t===f||"GeneratorFunction"===(t.displayName||t.name))},e.mark=function(e){return Object.setPrototypeOf?Object.setPrototypeOf(e,v):(e.__proto__=v,u(e,i,"GeneratorFunction")),e.prototype=Object.create(y),e},e.awrap=function(e){return{__await:e}},g(b.prototype),b.prototype[o]=function(){return this},e.AsyncIterator=b,e.async=function(t,n,r,a,o){void 0===o&&(o=Promise);var i=new b(c(t,n,r,a),o);return e.isGeneratorFunction(n)?i:i.next().then((function(e){return e.done?e.value:i.next()}))},g(y),u(y,i,"Generator"),y[a]=function(){return this},y.toString=function(){return"[object Generator]"},e.keys=function(e){var t=[];for(var n in e)t.push(n);return t.reverse(),function n(){for(;t.length;){var r=t.pop();if(r in e)return n.value=r,n.done=!1,n}return n.done=!0,n}},e.values=D,S.prototype={constructor:S,reset:function(e){if(this.prev=0,this.next=0,this.sent=this._sent=void 0,this.done=!1,this.delegate=null,this.method="next",this.arg=void 0,this.tryEntries.forEach(k),!e)for(var t in this)"t"===t.charAt(0)&&n.call(this,t)&&!isNaN(+t.slice(1))&&(this[t]=void 0)},stop:function(){this.done=!0;var e=this.tryEntries[0].completion;if("throw"===e.type)throw e.arg;return this.rval},dispatchException:function(e){if(this.done)throw e;var t=this;function r(n,r){return i.type="throw",i.arg=e,t.next=n,r&&(t.method="next",t.arg=void 0),!!r}for(var a=this.tryEntries.length-1;a>=0;--a){var o=this.tryEntries[a],i=o.completion;if("root"===o.tryLoc)return r("end");if(o.tryLoc<=this.prev){var u=n.call(o,"catchLoc"),c=n.call(o,"finallyLoc");if(u&&c){if(this.prev<o.catchLoc)return r(o.catchLoc,!0);if(this.prev<o.finallyLoc)return r(o.finallyLoc)}else if(u){if(this.prev<o.catchLoc)return r(o.catchLoc,!0)}else{if(!c)throw new Error("try statement without catch or finally");if(this.prev<o.finallyLoc)return r(o.finallyLoc)}}}},abrupt:function(e,t){for(var r=this.tryEntries.length-1;r>=0;--r){var a=this.tryEntries[r];if(a.tryLoc<=this.prev&&n.call(a,"finallyLoc")&&this.prev<a.finallyLoc){var o=a;break}}o&&("break"===e||"continue"===e)&&o.tryLoc<=t&&t<=o.finallyLoc&&(o=null);var i=o?o.completion:{};return i.type=e,i.arg=t,o?(this.method="next",this.next=o.finallyLoc,s):this.complete(i)},complete:function(e,t){if("throw"===e.type)throw e.arg;return"break"===e.type||"continue"===e.type?this.next=e.arg:"return"===e.type?(this.rval=this.arg=e.arg,this.method="return",this.next="end"):"normal"===e.type&&t&&(this.next=t),s},finish:function(e){for(var t=this.tryEntries.length-1;t>=0;--t){var n=this.tryEntries[t];if(n.finallyLoc===e)return this.complete(n.completion,n.afterLoc),k(n),s}},catch:function(e){for(var t=this.tryEntries.length-1;t>=0;--t){var n=this.tryEntries[t];if(n.tryLoc===e){var r=n.completion;if("throw"===r.type){var a=r.arg;k(n)}return a}}throw new Error("illegal catch attempt")},delegateYield:function(e,t,n){return this.delegate={iterator:D(e),resultName:t,nextLoc:n},"next"===this.method&&(this.arg=void 0),s}},e}(e.exports);try{regeneratorRuntime=r}catch(a){Function("r","regeneratorRuntime = r")(r)}},o0o1:function(e,t,n){e.exports=n("ls82")},wsfE:function(e,t,n){"use strict";var r=n("dI71"),a=n("q1tI"),o=n.n(a),i=n("2W6z"),u=n.n(i),c=Math.sqrt(50),l=Math.sqrt(10),s=Math.sqrt(2),d=function(e,t,n){var r,a,o,i,u=-1;if(n=+n,(e=+e)===(t=+t)&&n>0)return[e];if((r=t<e)&&(a=e,e=t,t=a),0===(i=function(e,t,n){var r=(t-e)/Math.max(0,n),a=Math.floor(Math.log(r)/Math.LN10),o=r/Math.pow(10,a);return a>=0?(o>=c?10:o>=l?5:o>=s?2:1)*Math.pow(10,a):-Math.pow(10,-a)/(o>=c?10:o>=l?5:o>=s?2:1)}(e,t,n))||!isFinite(i))return[];if(i>0){var d=Math.round(e/i),f=Math.round(t/i);for(d*i<e&&++d,f*i>t&&--f,o=new Array(a=f-d+1);++u<a;)o[u]=(d+u)*i}else{i=-i;var v=Math.round(e*i),h=Math.round(t*i);for(v/i<e&&++v,h/i>t&&--h,o=new Array(a=h-v+1);++u<a;)o[u]=(v+u)/i}return r&&o.reverse(),o};function f(e,t){if(!(e instanceof t))throw new TypeError("Cannot call a class as a function")}function v(e,t){for(var n=0;n<t.length;n++){var r=t[n];r.enumerable=r.enumerable||!1,r.configurable=!0,"value"in r&&(r.writable=!0),Object.defineProperty(e,r.key,r)}}function h(e,t,n){return t&&v(e.prototype,t),n&&v(e,n),e}function p(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function m(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function y(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?m(Object(n),!0).forEach((function(t){p(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):m(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function g(e,t){if("function"!=typeof t&&null!==t)throw new TypeError("Super expression must either be null or a function");e.prototype=Object.create(t&&t.prototype,{constructor:{value:e,writable:!0,configurable:!0}}),t&&E(e,t)}function b(e){return(b=Object.setPrototypeOf?Object.getPrototypeOf:function(e){return e.__proto__||Object.getPrototypeOf(e)})(e)}function E(e,t){return(E=Object.setPrototypeOf||function(e,t){return e.__proto__=t,e})(e,t)}function w(e){if(void 0===e)throw new ReferenceError("this hasn't been initialised - super() hasn't been called");return e}function k(e,t){return!t||"object"!=typeof t&&"function"!=typeof t?w(e):t}function S(e){var t=function(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],(function(){}))),!0}catch(e){return!1}}();return function(){var n,r=b(e);if(t){var a=b(this).constructor;n=Reflect.construct(r,arguments,a)}else n=r.apply(this,arguments);return k(this,n)}}function D(e,t){return function(e){if(Array.isArray(e))return e}(e)||function(e,t){if("undefined"==typeof Symbol||!(Symbol.iterator in Object(e)))return;var n=[],r=!0,a=!1,o=void 0;try{for(var i,u=e[Symbol.iterator]();!(r=(i=u.next()).done)&&(n.push(i.value),!t||n.length!==t);r=!0);}catch(c){a=!0,o=c}finally{try{r||null==u.return||u.return()}finally{if(a)throw o}}return n}(e,t)||O(e,t)||function(){throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}()}function C(e){return function(e){if(Array.isArray(e))return M(e)}(e)||function(e){if("undefined"!=typeof Symbol&&Symbol.iterator in Object(e))return Array.from(e)}(e)||O(e)||function(){throw new TypeError("Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}()}function O(e,t){if(e){if("string"==typeof e)return M(e,t);var n=Object.prototype.toString.call(e).slice(8,-1);return"Object"===n&&e.constructor&&(n=e.constructor.name),"Map"===n||"Set"===n?Array.from(e):"Arguments"===n||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)?M(e,t):void 0}}function M(e,t){(null==t||t>e.length)&&(t=e.length);for(var n=0,r=new Array(t);n<t;n++)r[n]=e[n];return r}var T="react-compound-slider:";function x(){var e=arguments.length>0&&void 0!==arguments[0]&&arguments[0];return function(t,n){return t.val>n.val?e?-1:1:n.val>t.val?e?1:-1:0}}function P(e,t,n){var r=arguments.length>3&&void 0!==arguments[3]&&arguments[3],a=e.findIndex((function(e){return e.key===t}));if(-1!==a){var o=e[a],i=o.key,u=o.val;return u===n?e:[].concat(C(e.slice(0,a)),[{key:i,val:n}],C(e.slice(a+1))).sort(x(r))}return e}function j(e,t){if(!e)return[0,0];var n=e.getBoundingClientRect();return[t?n.top:n.left,t?n.bottom:n.right]}function I(e){var t=e.type,n=void 0===t?"":t,r=e.touches;return!r||r.length>1||"touchend"===n.toLowerCase()&&r.length>0}function L(e,t){return e?t.touches[0].clientY:t.touches[0].pageX}function R(){var e=arguments.length>0&&void 0!==arguments[0]?arguments[0]:[],t=arguments.length>1?arguments[1]:void 0,n=arguments.length>2?arguments[2]:void 0,r=arguments.length>3?arguments[3]:void 0,a=0,o=e.map((function(e){var t=n.getValue(e);return e!==t&&(a+=1,u()(!r,"".concat(T," Invalid value encountered. Changing ").concat(e," to ").concat(t,"."))),t})).map((function(e,t){return{key:"$$-".concat(t),val:e}})).sort(x(t));return{handles:o,changes:a}}function A(e,t){return t}function H(e,t){for(var n=0;n<e.length;n++){if(e[n].key!==t[n].key)return e;if(t[n+1]&&t[n].val===t[n+1].val)return e}return t}function V(e,t,n,r,a){for(var o=-1,i=!0,u=0;u<e.length;u++){var c=e[u],l=t[u];if(!l||l.key!==c.key)return e;l.val!==c.val&&(o=u,i=l.val-c.val>0)}if(-1===o)return e;for(var s=i?n:-n,d=0;d<t.length;d++){var f=t[d],v=t[d+1];if(v&&f.val===v.val){if(d===o){var h=v.val+s;if(a(h)===h){var p=V(t,P(t,v.key,v.val+s,r),n,r,a);return p===t?e:p}return e}var m=f.val+s;if(a(m)===m){var y=V(t,P(t,f.key,f.val+s,r),n,r,a);return y===t?e:y}return e}}return t}function _(){for(var e=arguments.length,t=new Array(e),n=0;n<e;n++)t[n]=arguments[n];return function(e){return t.forEach((function(t){return t&&t(e)}))}}var N=function(){return{value:0,percent:0}},U=function(e){g(n,e);var t=S(n);function n(){var e;f(this,n);for(var r=arguments.length,a=new Array(r),o=0;o<r;o++)a[o]=arguments[o];return(e=t.call.apply(t,[this].concat(a))).getRailProps=function(){var t=arguments.length>0&&void 0!==arguments[0]?arguments[0]:{},n=e.props,r=n.emitMouse,a=n.emitTouch;return y(y({},t),{},{onMouseDown:_(t&&t.onMouseDown,r),onTouchStart:_(t&&t.onTouchStart,a)})},e}return h(n,[{key:"render",value:function(){var e=this.getRailProps,t=this.props,n=t.getEventData,r=t.activeHandleID,a=void 0===r?"":r,i=(0,t.children)({getEventData:n||N,activeHandleID:a,getRailProps:e});return i&&o.a.Children.only(i)}}]),n}(a.Component),F=function(e){g(n,e);var t=S(n);function n(){var e;f(this,n);for(var r=arguments.length,a=new Array(r),o=0;o<r;o++)a[o]=arguments[o];return(e=t.call.apply(t,[this].concat(a))).autofocus=function(e){e.target instanceof HTMLElement&&e.target.focus()},e.getHandleProps=function(t){var n=arguments.length>1&&void 0!==arguments[1]?arguments[1]:{},r=e.props,a=r.emitKeyboard,o=r.emitMouse,i=r.emitTouch;return y(y({},n),{},{onKeyDown:_(n&&n.onKeyDown,(function(e){return a&&a(e,t)})),onMouseDown:_(n&&n.onMouseDown,e.autofocus,(function(e){return o&&o(e,t)})),onTouchStart:_(n&&n.onTouchStart,(function(e){return i&&i(e,t)}))})},e}return h(n,[{key:"render",value:function(){var e=this.getHandleProps,t=this.props,n=t.activeHandleID,r=void 0===n?"":n,a=t.children,i=t.handles,u=a({handles:void 0===i?[]:i,activeHandleID:r,getHandleProps:e});return u&&o.a.Children.only(u)}}]),n}(a.Component),q=function(){function e(){f(this,e),this.interpolator=void 0,this.domain=[0,1],this.range=[0,1],this.domain=[0,1],this.range=[0,1],this.interpolator=null}return h(e,[{key:"createInterpolator",value:function(e,t){var n=this,r=e[0],a=e[1],o=t[0],i=t[1];return a<r?function(e){return n.interpolateValue(i,o)(n.deinterpolateValue(a,r)(e))}:function(e){return n.interpolateValue(o,i)(n.deinterpolateValue(r,a)(e))}}},{key:"interpolateValue",value:function(e,t){return t-=e=+e,function(n){return e+t*n}}},{key:"deinterpolateValue",value:function(e,t){return(t-=e=+e)?function(n){return(n-e)/t}:function(){return t}}},{key:"rescale",value:function(){return this.interpolator=null,this}},{key:"getValue",value:function(e){var t=this.domain,n=this.range;return(this.interpolator||(this.interpolator=this.createInterpolator(t,n)))(+e)}},{key:"setDomain",value:function(e){return this.domain=[e[0],e[1]],this.rescale(),this}},{key:"getDomain",value:function(){return this.domain}},{key:"setRange",value:function(e){return this.range=[e[0],e[1]],this}},{key:"getRange",value:function(){return this.range}},{key:"getTicks",value:function(e){var t=this.domain;return d(t[0],t[t.length-1],e||10)}}]),e}(),G=function(){return{value:0,percent:0}},z=function(e){g(n,e);var t=S(n);function n(){return f(this,n),t.apply(this,arguments)}return h(n,[{key:"render",value:function(){var e=this.props,t=e.children,n=e.values,r=e.scale,a=void 0===r?new q:r,i=e.count,u=void 0===i?10:i,c=e.getEventData,l=void 0===c?G:c,s=e.activeHandleID,d=t({getEventData:l,activeHandleID:void 0===s?"":s,ticks:(n||a.getTicks(u)).map((function(e){return{id:"$$-".concat(e),value:e,percent:a.getValue(e)}}))});return d&&o.a.Children.only(d)}}]),n}(a.Component),K=function(){return{value:0,percent:0}},B=function(e){g(n,e);var t=S(n);function n(){var e;f(this,n);for(var r=arguments.length,a=new Array(r),o=0;o<r;o++)a[o]=arguments[o];return(e=t.call.apply(t,[this].concat(a))).getTrackProps=function(t){var n=e.props,r=n.emitMouse,a=n.emitTouch;return y(y({},t||{}),{},{onMouseDown:_(t&&t.onMouseDown,r),onTouchStart:_(t&&t.onTouchStart,a)})},e}return h(n,[{key:"render",value:function(){for(var e=this.getTrackProps,t=this.props,n=t.children,r=t.left,a=void 0===r||r,i=t.right,u=void 0===i||i,c=t.scale,l=void 0===c?new q:c,s=t.handles,d=void 0===s?[]:s,f=t.getEventData,v=void 0===f?K:f,h=t.activeHandleID,p=void 0===h?"":h,m=l.getDomain(),y=[],g=0;g<d.length+1;g++){var b=d[g-1],E=d[g];0===g&&!0===a?b={id:"$",value:m[0],percent:0}:g===d.length&&!0===u&&(E={id:"$",value:m[1],percent:100}),b&&E&&y.push({id:"".concat(b.id,"-").concat(E.id),source:b,target:E})}var w=n({getEventData:v,activeHandleID:p,tracks:y,getTrackProps:e});return w&&o.a.Children.only(w)}}]),n}(a.Component);function X(e,t,n){return Math.min(Math.max(e,t),n)}var Y=function e(){var t=this;f(this,e),this.step=1,this.domain=[0,1],this.range=[0,1],this.setDomain=function(e){return t.domain=[e[0],e[1]],t},this.setRange=function(e){return t.range=[e[0],e[1]],t},this.setStep=function(e){return t.step=e,t},this.getValue=function(e){var n=D(t.domain,2),r=n[0],a=n[1],o=D(t.range,2),i=o[0],u=o[1],c=t.step,l=(X(e,r,a)-r)/(a-r);return X(c*Math.round(l*(u-i)/c)+i,i<u?i:u,u>i?u:i)}},$="undefined"!=typeof window&&"undefined"!=typeof document,J=function(){},W=function(e,t,n,r){var a=r?e-t:e+t;return r?Math.max(n[0],a):Math.min(n[1],a)},Z=function(e,t,n,r){var a=r?e+t:e-t;return r?Math.min(n[1],a):Math.max(n[0],a)},Q=[0,100],ee=function(e){g(n,e);var t=S(n);function n(){var e;f(this,n);for(var r=arguments.length,a=new Array(r),i=0;i<r;i++)a[i]=arguments[i];return(e=t.call.apply(t,[this].concat(a))).state={step:.1,values:[],domain:Q,handles:[],reversed:!1,activeHandleID:"",valueToPerc:null,valueToStep:null,pixelToStep:null},e.slider=o.a.createRef(),e.onKeyDown=function(t,n){var r=["ArrowRight","ArrowUp"],a=["ArrowDown","ArrowLeft"],o=w(e),i=o.state.handles,u=o.props,c=u.step,l=void 0===c?.1:c,s=u.reversed,d=void 0!==s&&s,f=u.vertical,v=void 0!==f&&f,h=u.domain,p=void 0===h?[0,100]:h,m=t.key||"".concat(t.keyCode);if(r.concat(a).includes(m)){if(v){var y=[a,r];r=y[0],a=y[1]}t.stopPropagation&&t.stopPropagation(),t.preventDefault&&t.preventDefault();var g=i.find((function(e){return e.key===n}));if(g){var b=g.val,E=b;r.includes(m)?E=W(b,l,p,d):a.includes(m)&&(E=Z(b,l,p,d));var k=i.map((function(e){return e.key===n?{key:e.key,val:E}:e}));e.submitUpdate(k,!0)}}},e.onMouseDown=function(t,n){e.onStart(t,n,!1)},e.onTouchStart=function(t,n){I(t)||e.onStart(t,n,!0)},e.getEventData=function(t,n){var r,a=w(e),o=a.state,i=o.pixelToStep,u=o.valueToPerc,c=a.props.vertical;return i.setDomain(j(e.slider.current,c)),n&&t instanceof TouchEvent?r=i.getValue(L(c,t)):t instanceof MouseEvent&&(r=i.getValue(c?t.clientY:t.pageX)),{value:r,percent:u.getValue(r)}},e.onMouseMove=function(t){var n=w(e),r=n.state,a=r.handles,o=r.pixelToStep,i=r.activeHandleID,u=void 0===i?"":i,c=n.props,l=c.vertical,s=c.reversed,d=void 0!==s&&s;o.setDomain(j(e.slider.current,l));var f=P(a,u,o.getValue(l?t.clientY:t.pageX),d);e.submitUpdate(f)},e.onTouchMove=function(t){var n=w(e),r=n.state,a=r.handles,o=r.pixelToStep,i=r.activeHandleID,u=n.props,c=u.vertical,l=u.reversed;if(null!==o&&!I(t)){o.setDomain(j(e.slider.current,c));var s=P(a,i,o.getValue(L(c,t)),l);e.submitUpdate(s)}},e.onMouseUp=function(){var t=w(e),n=t.state,r=n.handles,a=void 0===r?[]:r,o=n.activeHandleID,i=t.props,u=i.onChange,c=void 0===u?J:u,l=i.onSlideEnd,s=void 0===l?J:l;c(a.map((function(e){return e.val}))),s(a.map((function(e){return e.val})),{activeHandleID:o}),e.setState({activeHandleID:""}),$&&(document.removeEventListener("mousemove",e.onMouseMove),document.removeEventListener("mouseup",e.onMouseUp))},e.onTouchEnd=function(){var t=w(e),n=t.state,r=n.handles,a=n.activeHandleID,o=t.props,i=o.onChange,u=void 0===i?J:i,c=o.onSlideEnd,l=void 0===c?J:c;u(r.map((function(e){return e.val}))),l(r.map((function(e){return e.val})),{activeHandleID:a}),e.setState({activeHandleID:""}),$&&(document.removeEventListener("touchmove",e.onTouchMove),document.removeEventListener("touchend",e.onTouchEnd))},e}return h(n,[{key:"componentDidMount",value:function(){var e=this.state.pixelToStep,t=this.props.vertical;e.setDomain(j(this.slider.current,t))}},{key:"componentWillUnmount",value:function(){this.removeListeners()}},{key:"removeListeners",value:function(){$&&(document.removeEventListener("mousemove",this.onMouseMove),document.removeEventListener("mouseup",this.onMouseUp),document.removeEventListener("touchmove",this.onTouchMove),document.removeEventListener("touchend",this.onTouchEnd))}},{key:"onStart",value:function(e,t,n){var r=this.state.handles,a=this.props.onSlideStart,o=void 0===a?J:a;n||e.preventDefault&&e.preventDefault(),e.stopPropagation&&e.stopPropagation(),r.find((function(e){return e.key===t}))?(this.setState({activeHandleID:t}),o(r.map((function(e){return e.val})),{activeHandleID:t}),n?this.addTouchEvents():this.addMouseEvents()):(this.setState({activeHandleID:""}),this.handleRailAndTrackClicks(e,n))}},{key:"handleRailAndTrackClicks",value:function(e,t){var n,r=this,a=this.state,o=a.handles,i=a.pixelToStep,u=this.props,c=u.vertical,l=u.reversed,s=void 0!==l&&l,d=this.slider;i.setDomain(j(d.current,c)),n=t?i.getValue(L(c,e)):i.getValue(c?e.clientY:e.pageX);for(var f="",v=1/0,h=0;h<o.length;h++){var p=o[h],m=p.key,y=p.val,g=Math.abs(y-n);g<v&&(f=m,v=g)}var b=P(o,f,n,s);this.setState({activeHandleID:f},(function(){r.submitUpdate(b,!0),t?r.addTouchEvents():r.addMouseEvents()}))}},{key:"addMouseEvents",value:function(){$&&(document.addEventListener("mousemove",this.onMouseMove),document.addEventListener("mouseup",this.onMouseUp))}},{key:"addTouchEvents",value:function(){$&&(document.addEventListener("touchmove",this.onTouchMove),document.addEventListener("touchend",this.onTouchEnd))}},{key:"submitUpdate",value:function(e){var t=arguments.length>1&&void 0!==arguments[1]&&arguments[1],n=this.props,r=n.mode,a=void 0===r?1:r,o=n.step,i=void 0===o?.1:o,c=n.onUpdate,l=void 0===c?J:c,s=n.onChange,d=void 0===s?J:s,f=n.reversed,v=void 0!==f&&f,h=this.state.valueToStep.getValue;this.setState((function(n){var r=n.handles,o=[];if("function"==typeof a)o=a(r,e,i,v,h),u()(Array.isArray(o),"Custom mode function did not return an array.");else switch(a){case 1:o=A(0,e);break;case 2:o=H(r,e);break;case 3:o=V(r,e,i,v,h);break;default:o=e,u()(!1,"".concat(T," Invalid mode value."))}return l(o.map((function(e){return e.val}))),t&&d(o.map((function(e){return e.val}))),{handles:o}}))}},{key:"render",value:function(){var e=this,t=this.state,n=t.handles,r=t.valueToPerc,i=t.activeHandleID,u=this.props,c=u.className,l=u.rootStyle,s=void 0===l?{}:l,d=u.rootProps,f=void 0===d?{}:d,v=u.component,h=void 0===v?"div":v,p=u.disabled,m=void 0!==p&&p,g=u.flatten,b=void 0!==g&&g,E=n.map((function(e){var t=e.key,n=e.val;return{id:t,value:n,percent:r.getValue(n)}})),w=o.a.Children.map(this.props.children,(function(t){return!0===function(e){if(!Object(a.isValidElement)(e))return!1;var t=e.type,n=t?t.name:"";return n===F.name||n===U.name||n===z.name||n===B.name}(t)?o.a.cloneElement(t,{scale:r,handles:E,activeHandleID:i,getEventData:e.getEventData,emitKeyboard:m?J:e.onKeyDown,emitMouse:m?J:e.onMouseDown,emitTouch:m?J:e.onTouchStart}):t}));return b?o.a.createElement(o.a.Fragment,null,o.a.createElement(h,y(y({},f),{},{style:s,className:c,ref:this.slider})),w):o.a.createElement(o.a.Fragment,null,o.a.createElement(h,y(y({},f),{},{style:s,className:c,ref:this.slider}),w))}}],[{key:"getDerivedStateFromProps",value:function(e,t){var n,r,a=e.step,o=void 0===a?.1:a,i=e.values,c=e.domain,l=void 0===c?Q:c,s=e.reversed,d=void 0!==s&&s,f=e.onUpdate,v=void 0===f?J:f,h=e.onChange,p=void 0===h?J:h,m=e.warnOnChanges,y=void 0!==m&&m,g=t.valueToPerc,b=t.valueToStep,E=t.pixelToStep,w={};if(g&&b&&E||(g=new q,b=new Y,E=new Y,w.valueToPerc=g,w.valueToStep=b,w.pixelToStep=E),t.domain===Q||null===t.step||null===t.domain||null===t.reversed||o!==t.step||l[0]!==t.domain[0]||l[1]!==t.domain[1]||d!==t.reversed){var k=D(l,2),S=k[0],O=k[1];b.setStep(o).setRange([S,O]).setDomain([S,O]),!0===d?(g.setDomain([S,O]).setRange([100,0]),E.setStep(o).setRange([O,S])):(g.setDomain([S,O]).setRange([0,100]),E.setStep(o).setRange([S,O])),u()(O>S,"".concat(T," Max must be greater than min (even if reversed). Max is ").concat(O,". Min is ").concat(S,"."));var M=R(i||t.values,d,b,y),x=M.handles;(M.changes||void 0===i||i===t.values)&&(v(x.map((function(e){return e.val}))),p(x.map((function(e){return e.val})))),w.step=o,w.values=i,w.domain=l===Q?C(l):l,w.handles=x,w.reversed=d}else if(n=i,r=t.values,!(n===r||n.length===r.length&&n.reduce(function(e){return function(t,n,r){return t&&e[r]===n}}(r),!0))){var P=R(i,d,b,y),j=P.handles;P.changes&&(v(j.map((function(e){return e.val}))),p(j.map((function(e){return e.val})))),w.values=i,w.handles=j}return Object.keys(w).length?w:null}}]),n}(a.PureComponent),te={position:"absolute",height:"100%",width:24,left:8,transform:"translate(-50%, 0%)",borderRadius:7,cursor:"pointer"},ne={position:"absolute",height:"100%",width:10,left:8,transform:"translate(-50%, 0%)",borderRadius:4,pointerEvents:"none",backgroundColor:"#FDF6E3"};function re(e){var t=e.getRailProps;return o.a.createElement(a.Fragment,null,o.a.createElement("div",Object.assign({style:te},t())),o.a.createElement("div",{style:ne}))}function ae(e){var t=e.domain,n=t[0],r=t[1],a=e.handle,i=a.id,u=a.value,c=a.percent,l=e.getHandleProps;return o.a.createElement("button",Object.assign({role:"slider","aria-valuemin":n,"aria-valuemax":r,"aria-valuenow":u,className:"keyboardhandle",style:{top:c+"%",left:8,position:"absolute",transform:"translate(-50%, -50%)",width:24,height:10,zIndex:5,cursor:"pointer",border:4,borderRadius:"10%",boxShadow:"1px 1px 1px 1px rgba(0, 0, 0, 0.3)",backgroundColor:"#0044EE"}},l(i)))}function oe(e){var t=e.source,n=e.target,r=e.getTrackProps;return o.a.createElement("div",Object.assign({style:{position:"absolute",zIndex:1,backgroundColor:"#b28900",borderRadius:7,cursor:"pointer",width:14,transform:"translate(-50%, 0%)",top:t.percent+"%",left:8,height:n.percent-t.percent+"%"}},r()))}var ie=n("JI6e"),ue=n("3Z9Z");function ce(e){e.preventDefault()}function le(){document.body.addEventListener("touchmove",ce,{passive:!1})}function se(){document.body.removeEventListener("touchmove",ce)}var de={position:"relative",width:25,height:256,left:0,border:0,touchAction:"none"},fe=function(e){function t(t){var n;return(n=e.call(this,t)||this).onUpdate=function(e){n.setState({updates:e}),n.props.onChange(n.props.def.id,n.toVal(e[0]))},n.onChange=function(e){n.setState({values:e})},n.setValue=function(e){var t=n.state.values.slice(0);t[0]=n.fromVal(e),n.setState({values:t});var r=n.state.updates.slice(0);r[0]=n.fromVal(e),n.setState({updates:r}),n.sender.send(n.props.def.osc,e)},!0===n.props.def.log?(n.domain=[0,1],n.factor=Math.log(n.props.def.max/n.props.def.min),console.log(n.factor),n.props.def.step=.01):n.domain=[n.props.def.min,n.props.def.max],n.state={values:[n.fromVal(n.props.def.defval)],updates:[n.fromVal(n.props.def.defval)]},n}Object(r.a)(t,e);var n=t.prototype;return n.fromVal=function(e){return!0===this.props.def.log?Math.log(e/this.props.def.min)/this.factor:e},n.toVal=function(e){return!0===this.props.def.log?this.props.def.min*Math.exp(this.factor*e):e},n.componentDidMount=function(){},n.formatVal=function(e){return!0===this.props.def.log&&e>6?Math.floor(e):Math.floor(1e3*e)/1e3},n.render=function(){var e=this,t=this.state,n=t.values;t.updates;return o.a.createElement(ie.a,{className:"colfader"},o.a.createElement(ue.a,{noGutters:"true"},o.a.createElement(ie.a,null,this.props.def.label)),o.a.createElement(ue.a,{noGutters:"true"},o.a.createElement(ie.a,null,o.a.createElement(ee,{rootStyle:de,domain:this.domain,step:this.props.def.step,onUpdate:this.onUpdate,onChange:this.onChange,onSlideStart:le,onSlideEnd:se,values:n,vertical:!0,reversed:!0},o.a.createElement(U,null,(function(e){var t=e.getRailProps;return o.a.createElement(re,{getRailProps:t})})),o.a.createElement(F,null,(function(t){var n=t.handles,r=t.getHandleProps;return o.a.createElement("div",{className:"slider-handles"},n.map((function(t){return o.a.createElement(ae,{domain:e.domain,key:t.id,handle:t,getHandleProps:r})})))})),o.a.createElement(B,{left:!1,right:!1},(function(e){var t=e.tracks,n=e.getTrackProps;return o.a.createElement("div",{className:"slider-tracks"},t.map((function(e){var t=e.id,r=e.source,a=e.target;return o.a.createElement(oe,{key:t,source:r,target:a,getTrackProps:n})})))}))))),o.a.createElement(ue.a,{noGutters:"true"},o.a.createElement(ie.a,null,this.formatVal(this.toVal(this.state.updates[0])))))},t}(o.a.Component);t.a=fe}}]);
//# sourceMappingURL=component---src-pages-index-js-4ae3caa57f6a44a33758.js.map