// This file was automatically generated. Edits will be overwritten

export interface Typegen0 {
  "@@xstate/typegen": true;
  eventsCausingActions: {
    syncClock: "MANUAL_SYNC";
    rotateSecondHand: "xstate.after(1000)#clock.on.idle";
    rotateMinuteHand: "xstate.after(1000)#clock.on.idle";
    rotateHourHand: "xstate.after(1000)#clock.on.idle";
  };
  internalEvents: {
    "": { type: "" };
    "xstate.after(1000)#clock.on.idle": {
      type: "xstate.after(1000)#clock.on.idle";
    };
    "xstate.init": { type: "xstate.init" };
  };
  invokeSrcNameMap: {};
  missingImplementations: {
    actions: never;
    services: never;
    guards: never;
    delays: never;
  };
  eventsCausingServices: {};
  eventsCausingGuards: {};
  eventsCausingDelays: {};
  matchesStates:
    | "off"
    | "on"
    | "on.idle"
    | "on.tick"
    | { on?: "idle" | "tick" };
  tags: never;
}
