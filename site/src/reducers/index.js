import { REQUEST_CONSUMERS, RECEIVE_CONSUMERS, SELECT_CONSUMER } from '../actions'

const meatbarApp = (state, action) => {
  switch (action.type) {
    case REQUEST_CONSUMERS:
      return state;

    case RECEIVE_CONSUMERS:
      return transformPayload(state, action);

    case SELECT_CONSUMER:
      let selectedEater = state.meatbarEaters.find((eater) => eater.id == action.id);
      return Object.assign({}, state, { selectedEater });

    default:
      return state
  }
}

function transformPayload(state, action) {
  // consumedMeatbars:
  // preserve the response payload in normal form
  // for easy updates in the future
  let consumedMeatbars = action.consumption;

  let meatbarsByEater = groupByEater(consumedMeatbars);
  let meatbarEaters = Object.keys(meatbarsByEater).
    map((eaterId => buildConsumer(eaterId, meatbarsByEater)));

  // default selection
  let selectedEater = meatbarEaters[0];

  // The shape of the application state
  // should be preserved across all updates.
  // TODO: consider ImmutableJS here: https://facebook.github.io/immutable-js/
  return {
    selectedEater,
    meatbarEaters,
    consumedMeatbars,
    hasLoaded: true
  }
}

function buildConsumer(eaterId, meatbarsByEater) {
  let eater = meatbarsByEater[eaterId][0].eater;
  let id = eater.id;
  let meatbarsEaten = meatbarsByEater[eaterId];

  return {
    id,
    eater,
    meatbarsEaten
  }
};

function groupByEater(consumption) {
  return consumption.reduce( (acc, eatenMeatbar) => {
    let key = eatenMeatbar.eater.id;
    acc[key] = acc[key] || []
    acc[key].push(eatenMeatbar);
    return acc;
  }, {});
}

export default meatbarApp

