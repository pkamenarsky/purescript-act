var React = require('react');
var ReactTHREE = require('react-three');
var THREE = require('three');

exports.three = React.createClass({
  updateStateWithProps: function(props) {
    var geometry = null;
    var geometrywf = null;

    console.log(props);
    if (!props.geometry || props.geometry === "dodecahedron") {
      geometry = new THREE.DodecahedronGeometry(200, 1);
      geometrywf = new THREE.DodecahedronGeometry(205, 1);
    }
    else if (props.geometry === "cube") {
      geometry = new THREE.BoxGeometry(200, 200, 200);
      geometrywf = new THREE.BoxGeometry(205, 205, 205);
    }

    var material = new THREE.MeshBasicMaterial({
      color: 0x333333,
      wireframe: false
    });
    var materialwf = new THREE.MeshBasicMaterial({
      color: 0xffffff,
      wireframe: true
    });

    var cubepropswf = {};
    cubepropswf.geometry = geometrywf;
    cubepropswf.material = materialwf;

    var cubeprops = {};
    cubeprops.geometry = geometry;
    cubeprops.material = material;

    return { cameraazimuth: 0,
             geometry: geometry,
             geometrywf: geometrywf,
             cubeprops: cubeprops,
             cubepropswf: cubepropswf
           };
  },

  getInitialState: function() {
    return this.updateStateWithProps(this.props);
  },

  componentWillReceiveProps: function(props) {
    this.setState(this.updateStateWithProps(props));
  },

  componentDidMount: function() {
    var componentinstance = this;
    var start = null;
    var ref = componentinstance.refs.renderer;

    var animationcallback = function(t) {
      if (!start) {
        start = t;
      }
      var newazimuth = /*componentinstance.state.cameraazimuth*/ (t - start) * 0.0003;

      var newstate = {
        cameraazimuth: newazimuth,
        spincameracallback: requestAnimationFrame(animationcallback)
      };

      componentinstance.setState(newstate);
    };

    componentinstance.setState({
      spincameracallback: requestAnimationFrame(animationcallback),
      width: ref.clientWidth,
      height: ref.clientHeight
    });
  },

  componentWillUnmount: function() {
    if (this.state.spincameracallback !== null) {
      cancelAnimationFrame(this.state.spincameracallback);
    }
  },

  render: function() {
    var a = this.state.cameraazimuth;
    var MainCameraElement = React.createElement(
      ReactTHREE.PerspectiveCamera,
      { name:'maincamera',
        fov:'75',
        aspect:this.state.width/this.state.height,
        near:1,
        far:5000,
        position: new THREE.Vector3(Math.cos(a) * 600, 0, Math.sin(a) * 600),
        lookat: new THREE.Vector3(0,0,0)
      });

    return React.createElement(
      'div',
      {
        className: "fill",
        ref: "renderer",
        key: "renderer"
      },
      React.createElement(
        ReactTHREE.Renderer,
        { width: this.state.width,
          height: this.state.height,
          background: 0xffffff,
          key: "renderer2"
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
