import { REQUEST_CONSUMERS, RECEIVE_CONSUMERS, SELECT_CONSUMER } from '../actions';

const meatbarApp = (state, action) => {
  switch (action.type) {
  case REQUEST_CONSUMERS:
    return state;

  case RECEIVE_CONSUMERS:
    return transformPayload(state, action);

  case SELECT_CONSUMER:
    const selectedEater = state.meatbarEaters.find((eater) => eater.id == action.id);
    return Object.assign({}, state, { selectedEater });

  default:
    return state;
  }
};

function transformPayload(state, action) {
  const meatbarsByEater = groupByEater(action.consumption);
  const meatbarEaters = Object.keys(meatbarsByEater).
    map((eaterId => buildConsumer(eaterId, meatbarsByEater)));

  // default selection, may be undefined
  const selectedEater = meatbarEaters[0];

  // The shape of the application state
  // should be preserved across all updates.
  return {
    selectedEater,
    meatbarEaters,
    hasLoaded: true
  };
}

function buildConsumer(eaterId, meatbarsByEater) {
  const eater = meatbarsByEater[eaterId][0].eater;
  const id = eater.id;
  const meatbarsEaten = meatbarsByEater[eaterId];

  return {
    id,
    eater,
    meatbarsEaten
  };
}

function groupByEater(consumption) {
  return consumption.reduce((acc, eatenMeatbar) => {
    const key = eatenMeatbar.eater.id;
    acc[key] = acc[key] || [];
    acc[key].push(eatenMeatbar);
    return acc;
  }, {});
}

export default meatbarApp;
