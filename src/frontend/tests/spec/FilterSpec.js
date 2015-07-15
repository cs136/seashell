describe('Testing the Project Filter', function() {
  
  var $filter;
  
  beforeEach(inject(function(_$filter_){
    $filter = _$filter_;
  }));
  it('has a projectFilter', inject(function ($filter) {
    expect($filter('projectFilter')).not.toBeNull();
  }));
});
