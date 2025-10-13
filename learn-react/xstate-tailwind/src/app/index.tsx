import { useInterpret, useSelector } from "@xstate/react";

import { clock } from "~/machines";

export default function App() {
  const actor = useInterpret(clock);

  const secondHandDegree = useSelector(
    actor,
    ({ context }) => context.secondHandDegree
  );
  const minuteHandDegree = useSelector(actor, ({ context }) =>
    Math.floor(context.minuteHandDegree)
  );
  const hourHandDegree = useSelector(actor, ({ context }) =>
    Math.floor(context.hourHandDegree)
  );

  const { send } = actor;

  return (
    <div className="absolute-center space-y-5">
      <div
        className="
          w-[25rem] h-[25rem] rounded-full border-8 relative
          before:w-3.5 before:h-3.5 before:absolute-center before:z-10 before:bg-black before:rounded-full
        "
      >
        {Array.from(Array(12), (_, i) => (
          <div
            key={i}
            className="
              w-0.5 h-[calc(12.5rem-8px)] bg-black absolute-center !top-0 origin-bottom
              [clip-path:inset(0_0_calc(100%-theme(spacing.3)))]
            "
            style={{ transform: `translateX(-50%) rotate(${i * 30}deg)` }}
          />
        ))}
        <div
          className="w-2.5 h-[calc(12.5rem-8px-theme(spacing.20))] absolute-center !top-20 origin-bottom bg-black rounded-sm -z-10"
          style={{
            transform: `translateX(-50%) rotate(${hourHandDegree}deg)`,
          }}
        />
        <div
          className="w-1.5 h-[calc(12.5rem-8px-theme(spacing.8))] absolute-center !top-8 origin-bottom bg-black rounded-sm -z-10"
          style={{
            transform: `translateX(-50%) rotate(${minuteHandDegree}deg)`,
          }}
        />
        <div
          className="w-1 h-[calc(12.5rem-8px)] absolute-center !top-0 origin-bottom bg-red-500 rounded-sm -z-10"
          style={{
            transform: `translateX(-50%) rotate(${secondHandDegree}deg)`,
          }}
        />
      </div>
      <div className="space-x-2 flex justify-center">
        <button
          className="p-2 rounded border border-solid border-red-500 hover:bg-red-200 transition"
          onClick={() => send("TURN_OFF")}
        >
          TURN_OFF
        </button>
        <button
          className="p-2 rounded border border-solid border-red-500 hover:bg-red-200 transition"
          onClick={() => send("TURN_ON")}
        >
          TURN_ON
        </button>
        <button
          className="p-2 rounded border border-solid border-red-500 hover:bg-red-200 transition"
          onClick={() => send("MANUAL_SYNC")}
        >
          SYNC
        </button>
      </div>
    </div>
  );
}
