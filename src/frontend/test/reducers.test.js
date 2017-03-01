function action(type, payload){
  return {type: type, payload: payload};
}
describe("App States", () => {
  const appStateReducer = require('../src/reducers/appStateReducer');

  it('should update file contents when called', () => {
    const outputString = "Hello World";
    const expectedData = {
      currentProject:{
        currentQuestion:{
          currentFile: {
            content: outputString
          }
        }
      }
    };
    expect(appStateReducer.default({currentProject:{currentQuestion:{currentFile:{content: "fail"}}}}, action(appStateReducer.appStateActions.changeFileContent, outputString))
    ).toEqual(expectedData);
  });
});