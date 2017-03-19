// toaster.ts
import { Position, Toaster, Intent } from "@blueprintjs/core";

export const Errors = Toaster.create({
    className: "seashell-errors",
    position: Position.BOTTOM,
});

export default (message: string) => Errors.show({ message: message, timeout: 0, intent: Intent.DANGER });