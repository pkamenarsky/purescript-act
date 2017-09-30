var React = require('react');
var ReactTHREE = require('react-three');
var THREE = require('three');

exports.three = React.createClass({
  getInitialState: function() {
    var geometry = new THREE.BoxGeometry( 200,200,200);

    var geometry = new THREE.DodecahedronGeometry(200, 1);
    var geometrywf = new THREE.DodecahedronGeometry(205, 1);

    var material = new THREE.MeshBasicMaterial({
      color: 0x333333,
      wireframe: false,
    });
    var materialwf = new THREE.MeshBasicMaterial({
      color: 0xffffff,
      wireframe: true,
    });

    var cubepropswf = {};
    cubepropswf.geometry = geometrywf;
    cubepropswf.material = materialwf;

    var cubeprops = {};
    cubeprops.geometry = geometry;
    cubeprops.material = material;

    return { width: 1,
             height: 1,
             cameraazimuth: 0,
             geometry,
             geometrywf,
             cubeprops,
             cubepropswf,
           };
  },

  componentDidMount: function() {
    var componentinstance = this;
    var animationcallback = function(t) {
      let newazimuth = /*componentinstance.state.cameraazimuth*/ t * 0.0000001;
      // debug();
      let { clientHeight, clientWidth } = componentinstance.refs.renderer;
      console.log(clientHeight);

      let newstate = {
        cameraazimuth: newazimuth,
        spincameracallback: requestAnimationFrame(animationcallback),
        width: clientWidth,
        height: clientHeight
      };

      componentinstance.setState(newstate);
    };

    componentinstance.setState({spincameracallback:requestAnimationFrame(animationcallback)});
  },

  componentWillUnmount: function() {
    if (this.state.spincameracallback !== null) {
      cancelAnimationFrame(this.state.spincameracallback);
    }
    window.removeEventListener('resize',this.state.resizecallback);
  },

  render: function() {
    var MainCameraElement = React.createElement(
      ReactTHREE.PerspectiveCamera,
      { name:'maincamera',
        fov:'75',
        aspect:this.state.width/this.state.height,
        near:1,
        far:5000,
        position:new THREE.Vector3(0,0,600),
        lookat:new THREE.Vector3(0,0,0)
      });

    this.state.geometry.rotateY(this.state.cameraazimuth);
    this.state.geometrywf.rotateY(this.state.cameraazimuth);

    return React.createElement(
      'div',
      {
        className: "fill",
        ref: "renderer"
      },
      React.createElement(
        ReactTHREE.Renderer,
        { width: this.state.width,
          height: this.state.height,
          background: 0xffffff,
        },
        React.createElement(
          ReactTHREE.Scene,
          { width: this.state.width,
            height: this.state.height,
            camera:'maincamera'
          },
          MainCameraElement,
          React.createElement(ReactTHREE.Mesh, this.state.cubeprops)
        ),
        React.createElement(
          ReactTHREE.Scene,
          { width: this.state.width,
            height: this.state.height,
            camera:'maincamera'
          },
          MainCameraElement,
          React.createElement(ReactTHREE.Mesh, this.state.cubepropswf)
        )
      )
    );
  }});
