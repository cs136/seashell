import {OnlineCompiler} from "./Compiler/OnlineCompiler";
import {OfflineCompiler} from "./Compiler/OfflineCompiler";
import * from "./Compiler/Interface";
export * from "./Compiler/Interface";
export {Compiler};


class Compiler extends OnlineCompiler {
  constructor() {
    super(new OfflineCompiler());
  }
}
