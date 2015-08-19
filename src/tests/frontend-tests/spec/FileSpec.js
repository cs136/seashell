describe('File Controller', function() {
  beforeEach(module('frontend-app'));

  var $controller;
  beforeEach(inject(function(_$controller_){
    $controller = _$controller_;
  }));

  describe('Controller', function() {
    it('exists', function() {
      expect($controller).not.toBe(null);
    });
    it('is not undfined', function() {
      expect($controller).not.toBe(undefined);
    });
  });
});
