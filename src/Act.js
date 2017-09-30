var React = require('react');
var ReactTHREE = require('react-three');
var THREE = require('three');

exports.three = React.createClass({
  render: function() {
    // return React.createElement('div', null, "Hello world");
    this.props =
      { width: 300,
        height: 600,
        
      };

    var MainCameraElement = React.createElement(
      ReactTHREE.PerspectiveCamera,
      {name:'maincamera', fov:'75', aspect:this.props.width/this.props.height,
       near:1, far:5000,
       position:new THREE.Vector3(0,0,600), lookat:new THREE.Vector3(0,0,0)});

    var geometry = new THREE.BoxGeometry( 200,200,200);
    var geometry = new THREE.DodecahedronGeometry(200, 0);

    var material = new THREE.MeshBasicMaterial({
      color: 0xdddddd,
      wireframe: false,
    });
    var materialwf = new THREE.MeshBasicMaterial({
      color: 0xffffff,
      wireframe: true,
    });

    // var material = new THREE.MeshDepthMaterial({
    //   wireframe: true
    // });
    geometry.rotateY(1);

    var cubepropswf = {};
    cubepropswf.geometry = geometry;
    cubepropswf.position = THREE.Vector3(0, 0, 20);
    cubepropswf.material = materialwf;

    var cubeprops = {};
    cubeprops.geometry = geometry;
    cubeprops.position = THREE.Vector3(0, 0, 20);
    cubeprops.material = material;

    return React.createElement(
      ReactTHREE.Renderer,
      { width:this.props.width,
        height:this.props.height,
        background: 0xffffff,
      },
      React.createElement(
        ReactTHREE.Scene,
        {width:this.props.width, height:this.props.height, camera:'maincamera'},
        MainCameraElement,
        React.createElement(ReactTHREE.Mesh, cubeprops)
      ),
      React.createElement(
        ReactTHREE.Scene,
        {width:this.props.width, height:this.props.height, camera:'maincamera'},
        MainCameraElement,
        React.createElement(ReactTHREE.Mesh, cubepropswf)
      )
    );
  }});
