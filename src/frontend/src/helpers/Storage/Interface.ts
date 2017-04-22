const defaultSettings: Settings = {
  id: 0,
  editor_mode: "standard",
  font_size: 12,
  font: "Consolas",
  theme: "light",
  space_tab: true,
  tab_width: 2,
  offline_mode: 0,
};

export {AbstractStorage, AbstractWebStorage,
        File, FileID, FileBrief,
        Project, ProjectID, ProjectBrief,
        Settings, defaultSettings}


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
  public abstract async setOpenTabs(proj: ProjectID, question: string, files: FileBrief[]): Promise<void>;
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

class FileBrief {
  public id: FileID;
  public name: string; // a file name is (test|q*|common)/name
  public last_modified: number;
  public project: ProjectID;
  public checksum: string;
  public open: boolean;
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

class File extends FileBrief {
  public contents: string | undefined; // undefined ==> unavailable offline / unreadable
  public toFileBrief() {
    const obj = new FileBrief();
    obj.id = this.id;
    obj.name = this.name;
    obj.last_modified = this.last_modified;
    obj.project = this.project;
    obj.checksum = this.checksum;
    obj.open = this.open;
    return obj;
  }
}

// export const ext = (f: FileBrief) => {
//   let arr = f.name.split(".");
//   return arr.pop();
// };

// export const basename = (f: FileBrief) => {
//   let arr = f.name.split("/");
//   arr = arr[arr.length - 1].split(".");
//   arr.pop();
//   return arr.join(".");
// };

// export const fileQuestion = (f: FileBrief): string => {
//   let arr = f.name.split("/");
//   return arr[0];
// };

class ProjectBrief {
  public id: ProjectID;
  public name: string;
  public last_modified: number;
};

class Project extends ProjectBrief {
  public id: ProjectID;
  public name: string;
  public last_modified: number;
  public runs: {[index: string]: FileID};
  public open_tabs: {[index: string]: FileID};
}

class Settings {
  public id: 0;
  public editor_mode: string;
  public font_size: number;
  public font: string;
  public theme: "light"|"dark";
  public space_tab: boolean;
  public tab_width: number;
  public offline_mode: number;
}
