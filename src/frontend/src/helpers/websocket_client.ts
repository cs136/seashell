import {Coder} from './crypto';

interface Request {
  deferred: Promise<any>;
  callback: (type: string, result:any)=>any;
}

export class SeashellWebsocket {
  constructor(private uri: string, private key: Array<number>, private failure: ()=>void, private close: ()=>void) { }

  private coder: Coder;
  private lastRequest: number;
  public requests: Request[];
  public ready: Promise<boolean>;

  public async ping() { }
}
