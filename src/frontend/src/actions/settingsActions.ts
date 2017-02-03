export function updateSettings(newSettings: settingsReducerState){
  return {
    type: appStateActions.changeFont,
    payload: newSettings

  };
}
