export default function settingsChangeFontAction(font: string){
  return {
    type: appStateActions.changeFont,
    payload: settingsReducerState(font, 0, 0, 0, 0, 0)
  };
}
export default function settingsChangeFontSize(size: num){
  if(size>0){
    return{
      type: appStateActions.changeFontSize,
      payload: settingsReducerState('', size, 0, 0, 0, 0)
      };
  }
  else{
    return{
      type: appStateActions.nullAction
    };
  }
}
export default function settingsEditorModeChange(mode: num){
  return{
    type: appStateActions.changeEditorMode,
    payload: settingsReducerState('', 0, mode, 0, 0, 0)
  };
}
export default function settingsTabWidth(width: num){
  return{
    type: appStateActions.changeTabWidth,
    payload: settingsReducerState('', 0, 0, width, 0, 0)
  };
}
export default function settingsThemeChange(theme: num){
  return{
    type: appStateActions.changeWidth,
    payload: settingsReducerState('', 0, 0, 0, theme, 0)
  };
}
export default function settingsOfflineModeChange(mode: num){
  return{
    type: appStateActions.changeOfflineMode,
    payload: settingsReducerState('', 0, 0, 0, 0, mode)
  };
}
