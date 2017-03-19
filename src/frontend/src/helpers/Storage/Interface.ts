export {AbstractStorage,
        File, FileID, FileBrief,
        Project, ProjectID, ProjectBrief,
        Settings, defaultSettings}

abstract class AbstractStorage {
  // projects
  public abstract async newProject(name: string): Promise<void>;
  public abstract async getProject(proj: ProjectID): Promise<Project>;
  public abstract async getProjects(): Promise<ProjectBrief[]>;
  public abstract async deleteProject(proj: ProjectID): Promise<void>;
  public abstract async getFiles(proj: ProjectID): Promise<FileBrief[]>;
  // files
  public abstract async newFile(proj: ProjectID, filename: string, contents?: string): Promise<FileID>;
  public abstract async readFile(file: FileID): Promise<File>;
  public abstract async writeFile(file: FileID, contents: string): Promise<void>;
  public abstract async setFileToRun(proj: ProjectID, question: string, file: FileID): Promise<void>;
  public abstract async getFileToRun(proj: ProjectID, question: string): Promise<FileID|undefined>;
  public abstract async renameFile(file: FileID, newName: string): Promise<void>;
  public abstract async deleteFile(file: FileID): Promise<void>;
  // settings
  public abstract async setSettings(settings: Settings): Promise<void>;
  public abstract async getSettings(): Promise<Settings>;
}


type FileID = [string, string]; // compound key
type ProjectID = string; // alias of name for now

interface File extends FileBrief {
  id: FileID;
  name: string; // a file name is (test|q*|common)/name
  project: ProjectID;
  contents: string;
  checksum: string;
  last_modified: number;
}

interface FileBrief {
  id: FileID;
  name: string; // a file name is (test|q*|common)/name
  last_modified: number;
  project: ProjectID;
}

export const ext = (f: FileBrief) => {
  var arr = this.name.split(".");
  return arr.pop();
};

export const basename = (f: FileBrief) => {
  var arr = this.name.split(".");
  arr.pop();
  return arr.join(".");
};

interface Project extends ProjectBrief {
  id: ProjectID;
  name: string;
  last_modified: number;
  runs: {[index: string]: FileID};
  opened_tabs: {[index: string]: FileID};
}

interface ProjectBrief {
  id: ProjectID;
  name: string;
  last_modified: number;
};

interface Settings {
  id: 0,
  editor_mode: string,
  font_size: number;
  font: "Consolas";
  theme: "light"|"dark";
  space_tab: boolean;
  tab_width: number;
}

var defaultSettings: Settings = {
  id: 0,
  editor_mode: "standard",
  font_size: 12,
  font: "Consolas",
  theme: "light",
  space_tab: true,
  tab_width: 2
}
