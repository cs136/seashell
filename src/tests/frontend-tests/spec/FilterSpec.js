describe('Testing the Project Filter', function() {
  var $filter;
  beforeEach(module('frontend-app'));
 

  var testDict = [['A10', 1],['hi', 22],['bye', 3],['AA10', 4],
                  ['A2', 5],['Tut02', 6],['Tut02Sol', 7],
                  ['Lec90', 8],['Lec2', 9]]; 
  beforeEach(inject(function(_$filter_){
    $filter = _$filter_;
  }));
  it('has a projectFilter', function () {
    expect($filter('projectFilter')).not.toBeNull();
  });
  it('filters assignments', function () {
    var f = $filter('projectFilter');
    var fArray = f(testDict, 'A');
    expect(fArray.length).toEqual(2);
    expect(fArray[0]).toEqual('A10');
    expect(fArray[1]).toEqual('A2');
  });
  it('filters lectures', function () {
    var f = $filter('projectFilter');
    var fArray = f(testDict, 'LEC');
    expect(fArray.length).toEqual(2);
    expect(fArray[0]).toEqual('Lec90');
    expect(fArray[1]).toEqual('Lec2');
  });
  it('filters tutorials', function () {
    var f = $filter('projectFilter');
    var fArray = f(testDict, 'TUT');
    expect(fArray.length).toEqual(2);
    expect(fArray[0]).toEqual('Tut02');
    expect(fArray[1]).toEqual('Tut02Sol');
  });
  it('filters other', function () {
    var f = $filter('projectFilter');
    var fArray = f(testDict);
    expect(fArray.length).toEqual(3);
    expect(fArray[0]).toEqual('hi');
    expect(fArray[1]).toEqual('bye');
    expect(fArray[2]).toEqual('AA10');
  });
});
