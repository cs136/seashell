function action(type, payload){
  return {type: type, payload: payload};
}
describe("App States", () => {
  const appStateReducer = require('../src/reducers/appStateReducer');

  it('should update file contents when called', () => {
    const outputString = "Hello World";
    const expectedData = {currentProject:
      {currentQuestion:
        {currentFile:
          {contents: outputString,
            flusher: null,
            target: null,
            unwrittenContent: null}}}};
    expect(appStateReducer.default({currentProject:
      {currentQuestion:
        {currentFile:
          {contents: "fail",
            flusher: 15,
            target: ["A","B"],
            unwrittenContent: "fail"}}}}, action(appStateReducer.appStateActions.changeFileContent, outputString))
    ).toEqual(expectedData);
  });
});

