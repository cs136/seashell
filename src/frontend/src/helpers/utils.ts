import {mergeWith} from "ramda";
export const mergeBetter: any = (a: any, b: any) => (typeof a === "object" && typeof b === "object") ? mergeWith(mergeBetter, a, b) : b;