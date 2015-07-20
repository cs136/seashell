describe('Testing the Project Filter', function() {
  var $filter;
  beforeEach(module('frontend-app'));
 
  beforeEach(inject(function(_$filter_){
    $filter = _$filter_;
  }));
  it('has a projectFilter', function () {
    expect($filter('projectFilter')).not.toBeNull();
  });
});
