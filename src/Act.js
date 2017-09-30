var React = require('react');
var ReactTHREE = require('react-three');
var THREE = require('three');

exports.three = function() {
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

  var cubeprops = {};
  var material = new THREE.MeshBasicMaterial({
    color: 0xbbbbbb,
    wireframe: true,
    linewidth: 1,
  });

  // var material = new THREE.MeshDepthMaterial({
  //   wireframe: true
  // });

  cubeprops.geometry = geometry;
  cubeprops.position = THREE.Vector3(0, 0, 20);
  cubeprops.material = material;
  geometry.rotateY(1);

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
    )
  );
}
