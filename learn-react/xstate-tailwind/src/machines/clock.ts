import { createMachine, assign } from "xstate";

export type ClockContext = typeof clockContext;
export const clockContext = {
  secondHandDegree: 0,
  minuteHandDegree: 0,
  hourHandDegree: 0,
};

export type ClockEvent =
  | { type: "TURN_ON" }
  | { type: "TURN_OFF" }
  | { type: "MANUAL_SYNC" };

export default /** @xstate-layout N4IgpgJg5mDOIC5QGMA2B7ZBrAdOgZvgMQAqAqgEoByA+gPJWKgAO6sAlgC7voB2TIAB6IAjAGYxOMSIAMMgGwAOAJwB2aQCYALPIA0IAJ6JtWnCPkWtijTcUBWO1oC+T-Wky4+OdhFRgigrCcAIacYDjB+GEATgAUsnIAlETu2Hi83r5gAqwc3HwCwghaDjgyYsoiiopKInaVyvpGCBpiqjhaysp25so6qjKKqhoubhhpXtzYRDlsXDz8SEKIWqrKOBqK4mKtqrKKMsp6hqKqijgqVfZiMloaZyWjIKmevKSUtHQAYl+zeQuFUQadZHeQSHRdNQ6OxNU7rORVeRaErybprJ4vIgAWQAglQyDiADI0ADKAE0qABhP7zApLIoAWmB52RijEyJkrTZbLEsIQshEODU4i04m65hqIyevHQEDgAheeEINPyi1AjOBOD2lQUawU7PufI0InaJrqrTsAw0SPEGPGr0yfhVAPpiAkgvKlVU8kcFi6Ii0Rpk7Uh8gF1tFynudo86RwUywzrp6sQ8g0dg2PqUkPTIhEfID8g6XT9m2U7qsMYmapAuVpNaKN3anpNPp0qMqgZOCHUxdBdhu6aGNiriaWddVgIQDIHWpEOvkerBd1UfIZQxwdjkOy2q29dRcLiAA */
createMachine(
  {
    context: clockContext,
    tsTypes: {} as import("./clock.typegen").Typegen0,
    schema: { context: {} as ClockContext, events: {} as ClockEvent },
    id: "clock",
    description: "clock signals simulation",
    initial: "on",
    states: {
      off: {
        on: {
          TURN_ON: {
            target: "on",
          },
        },
      },
      on: {
        initial: "idle",
        states: {
          idle: {
            after: {
              "1000": {
                target: "tick",
              },
            },
          },
          tick: {
            entry: ["rotateSecondHand", "rotateMinuteHand", "rotateHourHand"],
            always: {
              target: "idle",
            },
          },
        },
        on: {
          TURN_OFF: {
            target: "off",
          },
        },
      },
    },
    entry: "syncClock",
    on: {
      MANUAL_SYNC: {
        actions: "syncClock",
      },
    },
  },
  {
    actions: {
      rotateSecondHand: assign({
        secondHandDegree: ctx => (ctx.secondHandDegree + 6) % 360,
      }),
      rotateMinuteHand: assign({
        minuteHandDegree: ctx => (ctx.minuteHandDegree + 1 / 10) % 360,
      }),
      rotateHourHand: assign({
        hourHandDegree: ctx => (ctx.hourHandDegree + 1 / 120) % 360,
      }),
      syncClock: assign(() => {
        const todayMidnight = (() => {
          const today = new Date();
          return today.setHours(0, 0, 0, 0);
        })();
        const secondsPassed = Math.floor((Date.now() - todayMidnight) / 1000);

        return {
          secondHandDegree: (secondsPassed % 60) * 6,
          minuteHandDegree: (secondsPassed % 3600) * (1 / 10),
          hourHandDegree: (secondsPassed % 86400) * (1 / 120),
        };
      }),
    },
  }
);
