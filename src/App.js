'use strict';

require('./style.css');
// require('./Act.purs').main();

if (module.hot) {
    module.hot.accept();
}

window.demo = require('./Act.purs').demo;
