import * as R from "ramda";

export {AbstractStorage, AbstractWebStorage,
        File, FileID, FileBrief, FileStored,
        Project, ProjectID, ProjectBrief, ProjectStored,
        Settings, SettingsStored,
        OfflineMode}

enum OfflineMode { Off, On, Forced }

abstract class AbstractStorage {
  // projects
  public abstract async newProject(name: string): Promise<ProjectBrief>;
  public abstract async getProject(proj: ProjectID): Promise<Project>;
  public abstract async getProjects(): Promise<ProjectBrief[]>;
  public abstract async deleteProject(proj: ProjectID): Promise<void>;
  public abstract async getProjectFiles(proj: ProjectID): Promise<FileBrief[]>;
  // files
  public abstract async newFile(proj: ProjectID, filename: string, contents?: string): Promise<FileBrief>;
  public abstract async readFile(file: FileID): Promise<File>;
  public abstract async writeFile(file: FileID, contents: string|undefined): Promise<void>;
  public abstract async renameFile(file: FileID, newName: string): Promise<FileBrief>;
  public abstract async deleteFile(file: FileID): Promise<void>;
  // questions
  public abstract async setFileToRun(proj: ProjectID, question: string, filename: string): Promise<void>;
  public abstract async getFileToRun(proj: ProjectID, question: string): Promise<string|false>;
  public abstract async addOpenFile(proj: ProjectID, question: string, file: FileID): Promise<void>;
  public abstract async removeOpenFile(proj: ProjectID, question: string, file: FileID): Promise<void>;
  public abstract async getOpenFiles(proj: ProjectID, question: string): Promise<FileBrief[]>;

  // settings
  public abstract async setSettings(settings: Settings): Promise<void>;
  public abstract async getSettings(): Promise<Settings>;
  // table dump
  public abstract async getAllFiles(): Promise<FileBrief[]>;
}

abstract class AbstractWebStorage {
  public abstract async syncAll(): Promise<void>;
}

type FileID = string; // compound key
type ProjectID = string; // alias of name for now

// NOTE: File objects may not necessarily have a valid
// prototype chain.  Do _NOT_ use instanceof to test
// if something's a File object.
class File implements FileStored {
  public id: FileID;
  public name: string; // a file name is (test|q*|common)/name
  public last_modified: number;
  public project: ProjectID;
  public checksum: string;
  public open: boolean;
  public contents: string | undefined; // undefined ==> unavailable offline / unreadable

  constructor(obj: FileStored | File) {
    this.id = obj.id;
    this.name = obj.name;
    this.last_modified = obj.last_modified;
    this.project = obj.project;
    this.checksum = obj.checksum;
    this.contents = obj.contents;
  }

  public mergeIdFrom(target: FileBrief) {
    this.id = target.id;
    this.name = target.name;
  }
  public basename() {
    let arr = this.name.split("/");
    arr = arr[arr.length - 1].split(".");
    arr.pop();
    return arr.join(".");
  }
  public extension() {
    return this.name.split(".").pop();
  }
  public question() {
    return this.name.split("/")[0];
  }
  public clone() {
    return new File(this);
  }
}

class FileBrief extends File {
  public contents: string | undefined = undefined;
  constructor(obj: FileStored) {
    super(obj);
    this.contents = undefined;
  }
}

class Project implements ProjectStored {
  public id: ProjectID;
  public name: string;
  public last_modified: number;
  public settings: {[index: string]: string};
  constructor(obj: ProjectStored) {
    this.id = obj.id;
    this.name = obj.name;
    this.last_modified = obj.last_modified;
    this.settings = obj.settings;
  }
}

class ProjectBrief extends Project {
  constructor(obj: ProjectStored) {
    super(obj);
    this.settings = {};
  }
};

class Settings implements SettingsStored {
  public id: 0 = 0;
  public editor_mode: string = "standard";
  public font_size: number = 12;
  public font: string = "Consolas";
  public theme: "light"|"dark" = "light";
  public space_tab: boolean = true;
  public tab_width: number = 2;
  public static fromJSON(obj: any): Settings {
    const st = new Settings();
    st.editor_mode = obj.editor_mode || st.editor_mode ;
    st.font_size   = obj.font_size   || st.font_size   ;
    st.font        = obj.font        || st.font        ;
    st.theme       = obj.theme       || st.theme       ;
    st.space_tab   = obj.space_tab   || st.space_tab   ;
    st.tab_width   = obj.tab_width   || st.tab_width   ;
    return st;
  }
  public toJSON(): SettingsStored {
    return {
        id          : 0
      , editor_mode : this.editor_mode
      , font_size   : this.font_size
      , font        : this.font
      , theme       : this.theme
      , space_tab   : this.space_tab
      , tab_width   : this.tab_width
    };
  }
}

interface FileStored {
  id: FileID;
  name: string; // a file name is (test|q*|common)/name
  last_modified: number;
  project: ProjectID;
  checksum: string;
  // indexed db does not support boolean index key, so use 0 | 1 when saving/reading data
  // and very carefully convert it to boolean in File constructor!
  contents: string | undefined; // undefined ==> unavailable offline / unreadable
}

interface ProjectStored {
  id: ProjectID;
  name: string;
  last_modified: number;
  settings: {[index: string]: string};
}

interface SettingsStored {
  id: 0;
  editor_mode: string;
  font_size: number;
  font: string;
  theme: "light"|"dark";
  space_tab: boolean;
  tab_width: number;
}
