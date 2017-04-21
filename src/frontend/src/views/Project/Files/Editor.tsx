import * as React from "react";
import {CompilerDiagnostic} from "../../../helpers/Services";

const styles = require("../Project.scss");

export interface MonacoEditorProps {
  dirty?: boolean;
  editorWillMount?: Function;
  editorDidMount?: Function;
  width?: number | string;
  height?: number | string;
  value?: string;
  defaultValue?: string;
  language?: string;
  theme?: string;
  options?: any;
  onChange?: Function;
  requireConfig?: any;
  context?: any;
  diags?: CompilerDiagnostic[];
  readOnly?: boolean;
}

export default class MonacoEditor extends React.PureComponent<MonacoEditorProps, {}> {
  monacoContext: any;
  editor: any;
  decorations: string[];
  prevent_change: boolean;
  current_value?: string;
  container?: HTMLElement;

  public static defaultProps: Partial<MonacoEditorProps> = {
    dirty: false,
    width: "100%",
    height: "100%",
    value: undefined,
    defaultValue: "",
    language: "javascript",
    theme: "vs",
    options: {glyphMargin: true},
    editorDidMount: undefined,
    editorWillMount: undefined,
    onChange: undefined,
    requireConfig: {},
    context: undefined,
    diags: [],
    readOnly: false
  };

  constructor(props: MonacoEditorProps) {
    super(props);
    this.editor = null;
    this.prevent_change = false;
    this.current_value = props.value;
    this.monacoContext = this.props.context || window;
    this.container = undefined;
    this.decorations = [];
  }
  componentDidUpdate = (previous: MonacoEditorProps) => {
    // Update value if and only if it changed from previous prop OR
    // if dirty goes from true => false.
    if (this.props.value !== previous.value ||
        (previous.dirty && !this.props.dirty)) {
      // By the time componentDidUpdate triggers, the state of
      // the file in Redux should have settled to the value
      // we currently have in the editor.
      // Hence if this if test fails, we're switching files.
      if (this.current_value !== this.props.value) {
        this.current_value = this.props.value;

        if (this.editor) {
          this.prevent_change = true;
          this.editor.setValue(this.current_value);
          this.prevent_change = false;
        }
      }
    }

    if (this.props.language !== previous.language) {
      this.monacoContext.monaco.editor.setModelLanguage(this.editor.getModel(), this.props.language);
    }
  }
  editorWillMount = () => {
    this.props.editorWillMount && this.props.editorWillMount(this.monacoContext.monaco);
  }
  editorDidMount = () => {
    this.props.editorDidMount && this.props.editorDidMount(this.editor, this.monacoContext.monaco);
    this.editor.onDidChangeModelContent((event: Event) => {
      const value = this.editor.getValue();
      this.current_value = value;
      if (!this.prevent_change) {
        this.props.onChange && this.props.onChange(value, event);
      }
    });
  }
  componentWillUnmount = () => {
    this.editor && this.editor.dispose();
    this.editor = null;
  }
  componentDidMount = () => {
    if (this.container) // Always reachable
      this.loadMonaco(this.container);
  }
  loadMonaco = (container: HTMLElement) => {
    const { requireConfig } = this.props;
    const loaderUrl = requireConfig.url || "vs/loader.js";
    const context = this.monacoContext;
    const onGotAmdLoader = () => {
      if (context.__REACT_MONACO_EDITOR_LOADER_ISPENDING__) {
        // Do not use webpack
        if (requireConfig.paths && requireConfig.paths.vs) {
          context.require.config(requireConfig);
        }
      }

      // Load monaco
      context.require(["vs/editor/editor.main"], () => {
        this.initMonaco(container);
      });

      // Call the delayed callbacks when AMD loader has been loaded
      if (context.__REACT_MONACO_EDITOR_LOADER_ISPENDING__) {
        context.__REACT_MONACO_EDITOR_LOADER_ISPENDING__ = false;
        let loaderCallbacks = context.__REACT_MONACO_EDITOR_LOADER_CALLBACKS__;
        if (loaderCallbacks && loaderCallbacks.length) {
          let currentCallback = loaderCallbacks.shift();
          while (currentCallback) {
            currentCallback.fn.call(currentCallback.context);
            currentCallback = loaderCallbacks.shift();
          }
        }
      }
    };

    // Load AMD loader if necessary
    if (context.__REACT_MONACO_EDITOR_LOADER_ISPENDING__) {
      // We need to avoid loading multiple loader.js when there are multiple editors loading concurrently
      //  delay to call callbacks except the first one
      context.__REACT_MONACO_EDITOR_LOADER_CALLBACKS__ = context.__REACT_MONACO_EDITOR_LOADER_CALLBACKS__ || [];
      context.__REACT_MONACO_EDITOR_LOADER_CALLBACKS__.push({
        context: this,
        fn: onGotAmdLoader
      });
    } else {
      if (typeof context.require === "undefined") {
        let loaderScript = context.document.createElement("script");
        loaderScript.type = "text/javascript";
        loaderScript.src = loaderUrl;
        loaderScript.addEventListener("load", onGotAmdLoader);
        context.document.body.appendChild(loaderScript);
        context.__REACT_MONACO_EDITOR_LOADER_ISPENDING__ = true;
      } else {
        onGotAmdLoader();
      }
    }
  }

  setDiags(diags: CompilerDiagnostic[]) {
    if (this.editor) {
      this.decorations = this.editor.deltaDecorations(this.decorations,
        diags.map((d: CompilerDiagnostic) => {
          const classPrefix = d.error ? "Error" : "Warning";
          return {
            range: new this.monacoContext.monaco.Range(d.line, 0, d.line, 999),
            options: {
              isWholeLine: true,
              glyphMarginClassName: `monaco${classPrefix}Glyph`,
              className: "monacoDiagnosticLineBackground",
              hoverMessage: d.message,
              glyphMarginHoverMessage: d.message
            }
          };
        }));
    }
  }

  initMonaco = (container: HTMLElement) => {
    const value = this.props.value !== null ? this.props.value : this.props.defaultValue;
    const { language, theme, options } = this.props;

    this.editor = this.monacoContext.monaco.editor.create(container, {
      value,
      language,
      theme,
      ...options,
    });

    this.setDiags(this.props.diags || []);

    this.editorDidMount();
  }

  render() {
    const { width, height } = this.props;
    const fixedWidth = (width || "100%").toString().indexOf("%") !== -1 ? width : `${width}px`;
    const fixedHeight = (height || "100%").toString().indexOf("%") !== -1 ? height : `${height}px`;
    const style = {
      width: fixedWidth,
      height: fixedHeight,
    };

    this.setDiags(this.props.diags || []);

    return (<div style={style} ref={(container?: HTMLElement) => {
        this.container = container;
      }}/>);
  }

}
