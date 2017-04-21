import * as R from "ramda";

export {AbstractStorage, AbstractWebStorage,
        File, FileID, FileBrief, FileStored,
        Project, ProjectID, ProjectBrief, ProjectStored,
        Settings, SettingsStored}


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
  public abstract async renameFile(file: FileID, newName: string): Promise<void>;
  public abstract async deleteFile(file: FileID): Promise<void>;
  // questions
  public abstract async setFileToRun(proj: ProjectID, question: string, filename: string): Promise<void>;
  public abstract async getFileToRun(proj: ProjectID, question: string): Promise<string|false>;
  public abstract async addOpenTab(proj: ProjectID, question: string, file: FileID): Promise<void>;
  public abstract async removeOpenTab(proj: ProjectID, question: string, file: FileID): Promise<void>;
  public abstract async getOpenTabs(proj: ProjectID, question: string): Promise<FileBrief[]>;

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

class File implements FileStored {

  public id: FileID;
  public name: string; // a file name is (test|q*|common)/name
  public last_modified: number;
  public project: ProjectID;
  public checksum: string;
  public open: boolean;
  public contents: string | undefined; // undefined ==> unavailable offline / unreadable

  constructor(obj: FileStored) {
    this.id = obj.id;
    this.name = obj.name;
    this.last_modified = obj.last_modified;
    this.project = obj.project;
    this.checksum = obj.checksum;
    // indexed db does not support boolean index key, so use 0 | 1 when saving/reading data
    // and very carefully convert it to boolean in File constructor!
    this.open = obj.open === 1;
    this.contents = obj.contents;
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
  public runs: {[index: string]: FileID};
  public open_tabs: {[index: string]: FileID};
  constructor(obj: ProjectStored) {
    this.id = obj.id;
    this.name = obj.name;
    this.last_modified = obj.last_modified;
    this.runs = obj.runs;
    this.open_tabs = obj.open_tabs;
  }
}

class ProjectBrief extends Project {
  public runs: {} = {};
  public open_tabs: {} = {};
  constructor(obj: ProjectStored) {
    super(obj);
    this.runs = {};
    this.open_tabs = {};
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
}

interface FileStored {
  id: FileID;
  name: string; // a file name is (test|q*|common)/name
  last_modified: number;
  project: ProjectID;
  checksum: string;
  // indexed db does not support boolean index key, so use 0 | 1 when saving/reading data
  // and very carefully convert it to boolean in File constructor!
  open: 0 | 1 | boolean;
  contents: string | undefined; // undefined ==> unavailable offline / unreadable
}

interface ProjectStored {
  id: ProjectID;
  name: string;
  last_modified: number;
  runs: {[index: string]: FileID};
  open_tabs: {[index: string]: FileID};
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
