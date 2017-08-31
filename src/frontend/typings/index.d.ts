declare module 'offline-plugin/runtime';
declare module 'react-draggable';
declare module 'xterm';
declare module "md5";

// declare var require: {
//    <T>(path: string): T;
//    (paths: string[], callback: (...modules: any[]) => void): void;
//    ensure: (paths: string[], callback: (require: <T>(path: string) => T) => void) => void;
// };
declare const IS_BROWSER: boolean;
declare const PRODUCTION: boolean;
declare const VERSION: string;
declare const DEBUG: boolean;
declare const DOMAIN: string;
