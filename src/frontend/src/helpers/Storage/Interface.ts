import * as R from "ramda";

export {AbstractStorage,
        Contents, ContentsStored, ContentsID,
        File, FileID, FileEntry, FileStored,
        Project, ProjectID, ProjectStored,
        Settings, SettingsStored,
        OfflineMode}

enum OfflineMode { Off, On, Forced }

abstract class AbstractStorage {
  // projects
  public abstract async newProject(name: string): Promise<Project>;
  public abstract async getProject(pid: ProjectID): Promise<Project>;
  public abstract async getProjects(): Promise<Project[]>;
  public abstract async deleteProject(pid: ProjectID): Promise<void>;
  public abstract async updateLastUsed(pid: ProjectID): Promise<void>
  // files
  public abstract async newFile(pid: ProjectID, filename: string, contents?: string): Promise<FileEntry>;
  public abstract async readFile(file: FileID, contents?: boolean): Promise<FileEntry>;
  public abstract async getFiles(pid: ProjectID, question?: string, contents?: boolean): Promise<File[]>;
  public abstract async getFileByName(pid: ProjectID, filename: string): Promise<FileEntry|false>;
  public abstract async writeFile(file: FileID, contents: string|undefined): Promise<FileID>;
  public abstract async renameFile(pid: ProjectID, currentName: string, newName: string): Promise<FileEntry>;
  public abstract async deleteFile(pid: ProjectID, filename: string): Promise<void>;
  public abstract async getVersions(pid: ProjectID, filename: string): Promise<Contents[]>;
  // questions
  public abstract async getQuestions(pid: ProjectID): Promise<string[]>;
  public abstract async newQuestion(pid: ProjectID, question: string): Promise<void>;
  public abstract async deleteQuestion(pid: ProjectID, question: string): Promise<void>;
  public abstract async setFileToRun(pid: ProjectID, question: string, filename: string): Promise<void>;
  public abstract async getFileToRun(pid: ProjectID, question: string): Promise<string|false>;
  public abstract async addOpenFile(pid: ProjectID, question: string, filename: string): Promise<void>;
  public abstract async removeOpenFile(pid: ProjectID, question: string, filename: string): Promise<void>;
  public abstract async getOpenFiles(pid: ProjectID, question: string): Promise<string[]>;

  // settings
  public abstract async setSettings(settings: Settings): Promise<void>;
  public abstract async getSettings(): Promise<Settings>;
  // table dump
  public abstract async getAllFiles(): Promise<File[]>;
}

type UUID = string;
type ContentsID = UUID;
type FileID = UUID;
type ProjectID = UUID;

class Contents implements ContentsStored {
  id: ContentsID;
  project_id: ProjectID;
  filename: string;
  contents: string;
  time: number;

  constructor(id: ContentsID, obj: ContentsStored) {
    this.id = id;
    this.project_id = obj.project_id;
    this.filename = obj.filename;
    this.contents = obj.contents;
    this.time = obj.time;
  }
}

// NOTE: File objects may not necessarily have a valid
// prototype chain.  Do _NOT_ use instanceof to test
// if something's a File object.
class File {
  public name: string; // a file name is (question|common)(/tests)?/name
  public project_id: ProjectID;
  public contents_id: ContentsID | false; // false => directory
  public contents: Contents | false; // false => directory or contents not loaded
  public flags: number;

  constructor(obj: FileStored | File) {
    this.name = obj.name;
    this.project_id = obj.project_id;
    this.contents_id = obj.contents_id;
    this.contents = false;
    if (obj instanceof File) {
      this.contents = obj.contents;
    }
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

class FileEntry extends File {
  public id: FileID;

  constructor(obj: FileStored) {
    super(obj);
    this.id = obj.id as string;
  }

  public mergeIdFrom(target: FileEntry) {
    this.id = target.id;
    this.name = target.name;
  }
}

class Project implements ProjectStored {
  public id: ProjectID;
  public name: string;
  public last_used: number;
  public settings: {[index: string]: string}; // Read-only copy of settings

  constructor(obj: ProjectStored) {
    this.id = obj.id as ProjectID;
    this.name = obj.name;
    this.last_used = obj.last_used;
    this.settings = obj.settings;
  }
}

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

interface ContentsStored {
  id?: ContentsID;
  project_id: ProjectID;
  filename: FileID;
  contents: string;
  time: number;
}

interface FileStored {
  id?: FileID;
  project_id: ProjectID;
  name: string; // a file name is (test|q*|common)/name
  contents_id: ContentsID | false; // false => directory
  flags: number;
}

interface ProjectStored {
  id?: ProjectID;
  name: string;
  settings: {[index: string]: string};
  last_used: number;
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
