import { REQUEST_CONSUMERS, RECEIVE_CONSUMERS, SELECT_CONSUMER } from '../actions'

const meatbarApp = (state, action) => {
  switch (action.type) {
    case RECEIVE_CONSUMERS:
      return transformPayload(state, action);

    case REQUEST_CONSUMERS:
      return state;

    case SELECT_CONSUMER:
      let selectedEater = state.meatbarEaters.find((eater) => eater.id == action.id);
      return Object.assign({}, state, { selectedEater });

    default:
      return state
  }
}

function transformPayload(state, action) {
  let consumedMeatbars = action.consumption;
  let meatbarsByEater = groupByEater(consumedMeatbars);
  let meatbarEaters = Object.keys(meatbarsByEater).
    map((eaterId => buildConsumer(eaterId, meatbarsByEater)));
  let selectedEater = meatbarEaters[0];

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
    if(acc[key] == undefined) {
      acc[key] = [];
    }
    acc[key].push(eatenMeatbar);
    return acc;
  }, {});
}

export default meatbarApp

