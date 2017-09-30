var React = require('react');
var ReactTHREE = require('react-three');
var THREE = require('three');

exports.three = React.createClass({
  getInitialState: function() {
    return { width: 1,
             height: 1,
             cameraazimuth:0
           };
  },

  componentDidMount: function() {
    var componentinstance = this;
    var animationcallback = function(/*t*/) {
      let newazimuth = componentinstance.state.cameraazimuth + 0.01;
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

    var geometry = new THREE.BoxGeometry( 200,200,200);

    var geometry = new THREE.DodecahedronGeometry(200, 1);
    var geometrywf = new THREE.DodecahedronGeometry(210, 1);

    var material = new THREE.MeshBasicMaterial({
      color: 0x333333,
      wireframe: false,
    });
    var materialwf = new THREE.MeshBasicMaterial({
      color: 0xffffff,
      wireframe: true,
    });

    // var material = new THREE.MeshDepthMaterial({
    //   wireframe: true
    // });
    geometry.rotateY(this.state.cameraazimuth);
    geometrywf.rotateY(this.state.cameraazimuth);

    var cubepropswf = {};
    cubepropswf.geometry = geometrywf;
    cubepropswf.scale = THREE.Vector3(1.2, 1.2, 1.2);
    cubepropswf.material = materialwf;

    var cubeprops = {};
    cubeprops.geometry = geometry;
    cubeprops.material = material;

    return React.createElement(
      'div',
      {
        className: "overlay",
        ref: "renderer"
      },
      React.createElement(
        ReactTHREE.Renderer,
        { width:this.state.width,
          height:this.state.height,
          background: 0xffffff,
        },
        React.createElement(
          ReactTHREE.Scene,
          { width:this.state.width,
            height:this.state.height,
            camera:'maincamera'
          },
          MainCameraElement,
          React.createElement(ReactTHREE.Mesh, cubeprops)
        ),
        React.createElement(
          ReactTHREE.Scene,
          { width:this.state.width,
            height:this.state.height,
            camera:'maincamera'
          },
          MainCameraElement,
          React.createElement(ReactTHREE.Mesh, cubepropswf)
        )
      )
    );
  }});
