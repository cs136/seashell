declare module 'offline-plugin/runtime';
declare module 'react-monaco-editor';

declare var require: {
   <T>(path: string): T;
   (paths: string[], callback: (...modules: any[]) => void): void;
   ensure: (paths: string[], callback: (require: <T>(path: string) => T) => void) => void;
}; 
